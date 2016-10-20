{-# language DataKinds           #-}
{-# language KindSignatures      #-}
{-# language OverloadedLabels    #-}
{-# language TypeOperators       #-}
{-# language OverloadedStrings   #-}
{-# language RankNTypes          #-}
{-# language PolyKinds           #-}
{-# language TypeInType          #-}
{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, KindSignatures,
             TypeFamilies, UndecidableInstances, MultiParamTypeClasses,
             FlexibleInstances, GADTs, FlexibleContexts, ScopedTypeVariables,
             ConstraintKinds #-}

module Main where

import Bookkeeper
import Bookkeeper.Internal

import Data.Text (Text)
import Data.Type.Map (Combinable, Unionable, Map(..), Mapping(..), Nubable, Union, AsMap, IsMap, Member, Lookup, Var(..), Submap, submap, union)
import Data.Type.Set ((:++), Sort, )
import GHC.Types
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Exts
import Data.Proxy (Proxy(..))


-- User Schema
type User = Book '[ "userid"   :=> Integer
                  , "username" :=> Text
                  ]

-- some users
stepcut :: User
stepcut =
  emptyBook & #userid =: 1
            & #username =: "stepcut"

ddssff :: User
ddssff =
  emptyBook & #userid =: 2
            & #username =: "ddssff"


-- User table
users :: [User]
users = [ stepcut, ddssff ]


-- Todo Schema
type Todo = Book '[ "todoid" :=> Integer
                  , "ownerid" :=> Integer
                  , "todo"   :=> Text
                  ]


-- some todos
todo1 :: Todo
todo1 = emptyBook & #todoid =: 1
                  & #ownerid =: 1
                  & #todo   =: "implement relatable"

todo2 :: Todo
todo2 = emptyBook & #todoid =: 2
                  & #ownerid =: 2
                  & #todo   =: "whip it real good"


-- Todo Table
todos :: [Todo]
todos = [todo1, todo2]


-- | A list of keys (fields) to project
data Keys a where
  NoKeys :: Keys '[]
  AddKey :: Key (k :: Symbol) -> Keys ks -> Keys (k ': ks)

-- | Given a book and a field (key) calculate (f :=> v)
type family ProjField book field where
  ProjField '[]                f = TypeError (GHC.TypeLits.Text "Field not found.")
  ProjField ((f   :=> v) ': m) f = (f :=> v)
  ProjField ((any :=> v) ': m) f = ProjField m f

-- | Given a book and a list of fields (keys) calculate all the (f :=> v) values
type family ProjFields book fields where
  ProjFields book '[]    = '[]
  ProjFields book (f:fs) = ProjField book f : ProjFields book fs

-- | Specific a value to be used in a selection operation
data Val book f v where
  VLit :: v -> Val book Nothing v
  VField :: (IsMap book, Gettable f book v) => Key f -> Val book (Just f) v

-- | Binary operations that can be used in a selection operation
data BinOp book f1 f2 v where
  Eq :: (Eq v) => Val book f1 v -> Val book f2 v -> BinOp book f1 f2 v

-- | A relational query
data Query c where
  Table   :: (IsMap a) => [Book a] -> Query [Book a]
  Cross   :: (Unionable a b, IsMap a, IsMap b) => (Query [Book a]) -> (Query [Book b]) -> Query [Book (a :++ b)]
  Select  :: (IsMap a) => BinOp a k1 k2 v -> Query [Book a] -> Query [Book a]
  Project :: (IsMap a, Submap (AsMap (ProjFields a ks)) a) => Keys ks -> Query [Book a] -> Query [Book (ProjFields a ks)]

-- | the the value for a selection
getVal :: Val book f v -> Book book -> v
getVal v row =
  case v of
    (VLit l) -> l
    (VField k) -> get k row

-- | check if a row meets the selection criteria
select :: BinOp book f1 f a -> Book book -> Bool
select binOp row =
  case binOp of
    (Eq v1 v2) ->
      let v1' = getVal v1 row
          v2' = getVal v2 row
      in v1' == v2'

-- | execute the query
exec :: Query c -> c
exec q =
  case q of
    (Table books) -> books
    (Cross q1 q2) ->
      let b1 = exec q1
          b2 = exec q2
      in cross b1 b2
    (Select binOp q) ->
      let rows = exec q
      in filter (select binOp) rows
    (Project keys q) ->
      let rows = exec q
      in map (\(Book b) -> Book (submap b)) rows

-- | perform the cross / cartesian product on all the rows
cross  :: (Unionable a b, IsMap a, IsMap b) => [Book a] -> [Book b] -> [Book (a :++ b)]
cross bookAs bookBs =
  do bookA <- bookAs
     bookB <- bookBs
     pure $ cross' bookA bookB
       where
         cross'  :: (Unionable a b, IsMap a, IsMap b) => Book a -> Book b -> Book (a :++ b)
         cross' (Book bookA) (Book bookB) = Book (union bookA bookB)

-- | helper function to print a table
printTable :: (ShowHelper (Book' a), IsMap a) => [Book a] -> IO ()
printTable = mapM_ print

-- | some example queries
main :: IO ()
main =
  do putStrLn "users table"
     printTable $ exec $ Table users
     putStrLn "todos table"
     printTable $ exec $ Table todos
     putStrLn "users × todos"
     printTable $ exec $ Cross (Table users) (Table todos)
     putStrLn "σ_{#userid == #ownerid} (users × todos)"
     printTable $ exec $ Select (Eq (VField #userid) (VField #ownerid))
                          (Cross (Table users) (Table todos))
     putStrLn "π_{#username, #todo} (σ_{#userid == #ownerid} (users × todos))"
     printTable $ exec $ Project (AddKey #username (AddKey #todo NoKeys))
                          (Select (Eq (VField #userid) (VField #ownerid))
                           (Cross (Table users) (Table todos)))
     putStrLn "π_{#username, #todo} (σ_{#userid == 1} (σ_{#userid == #ownerid} (users × todos)))"
     printTable $ exec $ (Project (AddKey #username (AddKey #todo NoKeys))
                          (Select (Eq (VField #userid) (VLit 1))
                           (Select (Eq (VField #userid) (VField #ownerid))
                            (Cross (Table users) (Table todos)))))


-- Ye Olde Cruft, to be removed.

--     mapM_ print $ exec $ Select #userid (Equal 1) (Cross (Table users) (Table todos))
{-
     putStrLn "projection"
     mapM_ print $ exec $ Project (AddKey #username (AddKey #todo (AddKey #userid NoKeys))) (SelectK #userid #ownerid (Proxy :: Proxy Integer) (Cross (Table users) (Table todos)))
     putStrLn "select'"
     mapM_ print $ exec $ Project (AddKey #username (AddKey #todo (AddKey #userid NoKeys)))
                           (Select (Eq (VField #userid) (VField #ownerid))
                            (Cross (Table users) (Table todos)))
     putStrLn "select'"
     mapM_ print $ exec $ Project (AddKey #username (AddKey #todo (AddKey #userid NoKeys)))
                           (Select (Eq (VField #userid) (VLit 1))
                            (Cross (Table users) (Table todos)))
-}

--     mapM_ print $ exec $ SelectK #userid #ownerid (Proxy :: Proxy Integer) (Cross (Table users) (Table todos))
--     mapM_ print $ exec $ Project (Proxy :: Proxy '["userid"   :=> Integer]) (SelectK #userid #ownerid (Proxy :: Proxy Integer) (Cross (Table users) (Table todos)))
--     mapM_ print $ exec $ Proj'' (AddKey #userid (AddKey #username NoKeys)) (SelectK #userid #ownerid (Proxy :: Proxy Integer) (Cross (Table users) (Table todos)))

--     mapM_ print $ exec $ ProjectK (Proxy :: Proxy '["userid"]) (SelectK #userid #ownerid (Proxy :: Proxy Integer) (Cross (Table users) (Table todos)))


{-
select :: (IsMap a) => (Book a -> Bool) -> [Book a] -> [Book a]
select p bks = filter p bks

-- fef :: (IsMap a, Member k1 a ~ True, Member k2 a ~ True) => Key k1 -> Key k2 -> Book a -> Bool
-- fef :: forall a k1 k2 v1 v2. (IsMap a, Lookup a k1 ~ Lookup a k2, Gettable k1 a v1, Gettable k2 a v2, v1 ~ v2) => Key k1 -> Key k2 -> Book a -> Bool
-- fef :: (IsMap a, Lookup a k1 ~ Just v1, Lookup a k2 ~ Just v2, v1 ~ v2, Eq v1, Gettable k1 a v1, Gettable k2 a v2) => Key k1 -> Key k2 -> Book a -> Bool
fef :: (IsMap a, Gettable k1 a (Lookup a k1)) => Key k1 -> Key k2 -> Proxy x -> Book a -> Bool
fef k1 k2 book =
  let v1 = get k1 book
      v2 = get k2 book
  in v1 == v2
-}
{-
-- lookup :: (k1 ~ k2) => Var k1 -> Map ((Mapping k2 v) ': r) -> ()
lookup :: (k1 ~ k2) => Var k1 -> Map (((k2 :-> v) ': r) :: [Mapping Symbol *]) -> ()
lookup k m =
  case m of
--    Empty -> error "lookup: me fail? Impossible!"
    (Ext k' v map')
      | k == k' -> ()
-}

{-
  case book of
    Empty -> False
    (Ext k v map') -> False
-}
{-
    (Join k1 k2 q1 q2) ->
      let rows1 = exec q1
          rows2 = exec q2
      in []
-}
-- join :: Key k1 -> Key k2 -> Book a -> Book b -> 


{-
class DoProj'' ks a where
  doProj'' :: (IsMap a) => Keys ks -> Book a -> Book (ProjFields a ks)

instance DoProj'' '[] a where
  doProj'' _ book = Book Empty

instance (Gettable k a DoProj'' (k:ks) a where
  doProj'' _ book = Book Empty
-}


-- proj'' :: (IsMap a, Submap (ProjFields a ks) a) => Keys ks -> Book a -> Book (ProjFields a ks)
-- proj'' ks (Book book) = Book (submap book)


-- data Keys (a :: [Symbol]) = Keys

-- type family Project' (k :: *) (m :: [Mapping Symbol *]) :: [(Mapping Symbol *)] -- n
-- type instance Project' (Key k) ((k :-> v) ': vs) = '[(k :-> v)]
-- type instance Project' (Key k) ((k' :-> v) ': (k'' :-> v') ': vs) = Project' (Key k) ((k' :-> v') ': vs)

{-
class Proj k m n where
  proj :: k -> Map m -> Map n

instance Proj (Key k) ((k :-> v) ': vs) ((k :-> v) ': '[]) where
  proj _ (Ext k v xs) = Ext k v Empty

instance (Proj (Key k) vs vs) => Proj (Key k) ((k' :-> v) ': vs) vs where
  proj k (Ext _ _ xs) = proj k xs
-}
  
 {-
  SelectOld  :: (IsMap a, Gettable k a v) => Key k -> Op v -> Query [Book a] -> Query [Book a]
  SelectK :: (IsMap a, Gettable k1 a v1, Gettable k2 a v2, v1 ~ v, v2 ~ v, Eq v) => Key k1 -> Key k2 -> Proxy v -> Query [Book a] -> Query [Book a]
-}

--  ProjectOld :: (IsMap a, IsMap b, Submap b a) => Proxy b -> Query [Book a] -> Query [Book b]
--  Proj    :: (HasKeys a ks, IsMap a, IsMap b, Submap b a) => Keys ks -> Query [Book a] -> Query [Book b]
--  Proj'   :: (HasKey a k, IsMap a, IsMap b, Submap b a) => Key k -> Query [Book a] -> Query [Book b]


--  Proj''  :: Key ks -> Query [Book a] -> Query [Book (ProjRes a ks)]
--   ProjectK :: (IsMap a, IsMap b, Submap b a) => Fields f -> Query [Book a] -> Query [Book b]
{-
  
data Fields f where
  FZero :: Fields '[]
  Field :: Key f -> Fields fs -> Fields (f ': fs)
-}
{-
    (SelectOld k (Equal v) q) ->
      let rows = exec q
      in filter (\row -> get k row == v) rows
    (SelectK k1 k2 (p :: Proxy t) q) ->
      let rows = exec q      in filter (\row -> ((get k1 row) :: t) == ((get k2 row) :: t)) rows
    (ProjectOld _ q) ->
      let rows = exec q
      in map (\(Book m) -> Book (submap m)) rows
-}
{-
data Op v where
  Equal :: (Eq v) => v -> Op v

data List n where
  Nil :: List '[]
  Cons :: e -> List l -> List (e ': l)
-}

{-
class Project k m n where
  project :: Proxy k -> Map m -> Map n

instance Project '[] '[] '[] where
  project _ _ = Empty

instance Project ks vs vs => Project (k:ks) ((k :-> v) ': vs) ((k :-> v) ': vs) where
  project _ (Ext k v xs) = Ext k v (project (Proxy :: Proxy ks) xs)

instance Project ks vs vs => Project (k':ks) ((k :-> v) ': vs) vs where
  project _ (Ext _ _ xs) = project (Proxy :: Proxy ks) xs
-}
{-

-- Check that a book has a field
type HasKey book field = HasKey' book field book

type family HasKey' book field orig :: Constraint where
  HasKey' '[] field '[] = TypeError (GHC.TypeLits.Text "The provided Book is empty")
  HasKey' '[] field orig = TypeError (GHC.TypeLits.Text "The provided Book does not contain the field "
                                 :<>: ShowType field
                                 :$$: GHC.TypeLits.Text "Book type:"
                                 :$$: ShowType orig)
  HasKey' ((k :=> v) ': m) k orig = (Gettable k orig v)
  HasKey' (any ': m) k orig = HasKey' m k orig


-- Check that a book has all the fields
type family HasKeys book fields :: Constraint where
  HasKeys '[] fields = TypeError (GHC.TypeLits.Text "The provided Book is empty")
  HasKeys book '[] = ()
  HasKeys book (f ': fs) = (HasKey book f, HasKeys book fs)

-}
