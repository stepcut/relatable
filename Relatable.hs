{-# language DataKinds           #-}
{-# language DeriveDataTypeable  #-}
{-# language KindSignatures      #-}
{-# language OverloadedLabels    #-}
{-# language TypeOperators       #-}
{-# language OverloadedStrings   #-}
{-# language RankNTypes          #-}
{-# language PolyKinds           #-}
{-# language TypeInType          #-}
{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell     #-}
{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, KindSignatures,
             TypeFamilies, UndecidableInstances, MultiParamTypeClasses,
             FlexibleInstances, GADTs, FlexibleContexts, ScopedTypeVariables,
             ConstraintKinds #-}

module Main where

import Bookkeeper
import Bookkeeper.Internal

import Control.Monad.Reader (ask)
import Data.Acid  (Query, Update, makeAcidic, query, update)
import Data.Acid.Memory (openMemoryState)
import Data.Data (Data, Typeable)
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy, safePut, safeGet)
import Data.Text (Text)
import Data.Type.Map (Combinable, Unionable, Map(..), Mapping(..), Nubable, Union, AsMap, IsMap, Member, Lookup, Var(..), Submap, submap, union)
import Data.Type.Set ((:++), Sort, )
import GHC.Types
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Exts
import Data.Proxy (Proxy(..))

-- deriveSafeCopy 0 'base ''Book'

-- instance SafeCopy (Book' a) where
--  version =

instance SafeCopy (Map '[]) where
  putCopy Empty = contain $ pure ()
  getCopy = contain $ pure Empty

instance (SafeCopy v, SafeCopy (Map rs)) => SafeCopy (Map ((k :=> v) ': rs)) where
  putCopy (Ext k v rs) =
    contain $
      do safePut v
         safePut rs
  getCopy =
    contain $
      pure Ext <*> pure Var <*> safeGet <*> safeGet

instance (SafeCopy (Map a)) => SafeCopy (Book' a) where
  putCopy (Book m) =
    contain $ safePut m
  getCopy =
    contain $
      pure Book <*> safeGet


-- | A list of keys (fields) to project
data Keys a where
  NoKeys :: Keys '[]
  AddKey :: Key (k :: Symbol) -> Keys ks -> Keys (k ': ks)
{-
instance SafeCopy (Keys '[]) where
  putCopy = undefined
  getCopy = undefined

instance (SafeCopy (Keys ks)) => SafeCopy (Keys (k ': ks)) where
  getCopy = undefined
  putCopy =
    contain $ safePut ()
-}

-- | Given a book and a field (key) calculate (f :=> v)
type family ProjField book field where
  ProjField '[]                f = TypeError (GHC.TypeLits.Text "Field not found.")
  ProjField ((f   :=> v) ': m) f = (f :=> v)
  ProjField ((any :=> v) ': m) f = ProjField m f

-- | Given a book and a list of fields (keys) calculate all the (f :=> v) values
type family ProjFields book fields where
  ProjFields book '[]    = '[]
  ProjFields book (f:fs) = ProjField book f : ProjFields book fs

type family Project (ks :: [Symbol]) a where
  Project ks (Book' a) = Book (ProjFields a ks)

-- | Specific a value to be used in a selection operation
data Val book f v where
  VLit :: v -> Val book Nothing v
  VField :: (IsMap book, Gettable f book v) => Key f -> Val book (Just f) v

-- | Binary operations that can be used in a selection operation
data BinOp book f1 f2 v where
  Eq :: (Eq v) => Val book f1 v -> Val book f2 v -> BinOp book f1 f2 v

-- | A relational query
data Relation db c where
--  Table   :: (IsMap a) => [Book a] -> Relation [Book a]
  Table   :: (IsMap a) => (db -> [Book a]) -> Relation db [Book a]
  Cross   :: (Unionable a b, IsMap a, IsMap b) => (Relation db [Book a]) -> (Relation db [Book b]) -> Relation db [Book (a :++ b)]
  Select  :: (IsMap a) => BinOp a k1 k2 v -> Relation db [Book a] -> Relation db [Book a]
  Project :: (IsMap a, Submap (AsMap (ProjFields a ks)) a) => Keys ks -> Relation db [Book a] -> Relation db [Book (ProjFields a ks)]

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

type family Cross a b where
  Cross (Book' a) (Book' b) = Book (a :++ b)

-- | perform the cross / cartesian product on all the rows
cross  :: (Unionable a b, IsMap a, IsMap b) => [Book a] -> [Book b] -> [Book (a :++ b)]
cross bookAs bookBs =
  do bookA <- bookAs
     bookB <- bookBs
     pure $ cross' bookA bookB
       where
         cross'  :: (Unionable a b, IsMap a, IsMap b) => Book a -> Book b -> Book (a :++ b)
         cross' (Book bookA) (Book bookB) = Book (union bookA bookB)

-- | execute the query
exec :: db -> Relation db c -> c
exec db q =
  case q of
    (Table fn) -> fn db
    (Cross q1 q2) ->
      let b1 = exec db q1
          b2 = exec db q2
      in cross b1 b2
    (Select binOp q) ->
      let rows = exec db q
      in filter (select binOp) rows
    (Project keys q) ->
      let rows = exec db q
      in map (\(Book b) -> Book (submap b)) rows

querySt :: Relation db c -> Query db c
querySt rel =
  do db <- ask
     pure $ exec db rel

-- | helper function to print a table
printTable :: (ShowHelper (Book' a), IsMap a) => [Book a] -> IO ()
printTable = mapM_ print


-- * Example Usage

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


-- | the database / collection of tables
data Tables = Tables
  { users :: [User]
  , todos :: [Todo]
  }
  deriving (Eq, Show)


-- | deriveSafeCopy can not handle this yet
instance SafeCopy Tables where
  getCopy =
    contain $
     Tables <$> safeGet <*> safeGet
  putCopy (Tables u t) =
    contain $ do safePut u
                 safePut t

-- | initial database values
tables :: Tables
tables = Tables
  { users = [ stepcut, ddssff ]
  , todos = [todo1, todo2]
  }

-- ** Queries

-- | get all the Todos
getTodos :: Query Tables [Todo]
getTodos = querySt (Table todos)

-- | get all the Users
getUsers :: Query Tables [User]
getUsers = querySt (Table users)

-- | cartesian / cross product of User and Todo
crossUsersTodos :: Query Tables [Cross User Todo]
crossUsersTodos =
  querySt (Cross (Table users) (Table todos))

-- | select only the crossed rows where the userid == ownerid
selectUsersTodos :: Query Tables [Cross User Todo]
selectUsersTodos =
  querySt (Select
           (Eq (VField #userid) (VField #ownerid))
           (Cross (Table users) (Table todos)))

-- | cross the tables, select only the rows where userid==ownerid, and
-- then project just the "username" and "todo" fields
projectUsersTodos :: Query Tables [Project '["username", "todo"] (Cross User Todo)]
projectUsersTodos =
  querySt (Project (AddKey #username (AddKey #todo NoKeys))
           (Select (Eq (VField #userid) (VField #ownerid))
            (Cross (Table users) (Table todos))))

-- | find all the todos for userid == i
projectTodosFor :: Integer -> Query Tables [Project '["username", "todo"] (Cross User Todo)]
projectTodosFor i =
 querySt (Project (AddKey #username (AddKey #todo NoKeys))
          (Select (Eq (VField #userid) (VLit i))
           (Select (Eq (VField #userid) (VField #ownerid))
            (Cross (Table users) (Table todos)))))

makeAcidic ''Tables
  [ 'getTodos
  , 'getUsers
  , 'crossUsersTodos
  , 'selectUsersTodos
  , 'projectUsersTodos
  , 'projectTodosFor
  ]

-- | some example queries
main :: IO ()
main =
  do acid <- openMemoryState tables
     putStrLn "users table"
     res <- query acid GetUsers
     printTable res

     putStrLn "todos table"
     res <- query acid GetTodos
     printTable res

     putStrLn "users × todos"
     res <- query acid CrossUsersTodos
     printTable res

     putStrLn "σ_{#userid == #ownerid} (users × todos)"
     res <- query acid SelectUsersTodos
     printTable res

     putStrLn "π_{#username, #todo} (σ_{#userid == #ownerid} (users × todos))"
     res <- query acid ProjectUsersTodos
     printTable res

     putStrLn "π_{#username, #todo} (σ_{#userid == 1} (σ_{#userid == #ownerid} (users × todos)))"
     res <- query acid (ProjectTodosFor 1)
     printTable res
