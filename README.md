relatable
=========

This implements a pure, strongly typed relational algebra. It is currently a proof-of-concept implementation.

The type system should prevent you from doing stupid things like projecting non-existent fields or trying to perform a selection/restriction on fields of different types.

This example app:

```{haskell}
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
```

produces this output:

```
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :load Relatable.hs 
[1 of 1] Compiling Main             ( Relatable.hs, interpreted )
Ok, modules loaded: Main.
*Main> main
users table
Book {userid = 1, username = "stepcut"}
Book {userid = 2, username = "ddssff"}
todos table
Book {ownerid = 1, todo = "implement relatable", todoid = 1}
Book {ownerid = 2, todo = "whip it real good", todoid = 2}
users × todos
Book {ownerid = 1, todo = "implement relatable", todoid = 1, userid = 1, username = "stepcut"}
Book {ownerid = 2, todo = "whip it real good", todoid = 2, userid = 1, username = "stepcut"}
Book {ownerid = 1, todo = "implement relatable", todoid = 1, userid = 2, username = "ddssff"}
Book {ownerid = 2, todo = "whip it real good", todoid = 2, userid = 2, username = "ddssff"}
σ_{#userid == #ownerid} (users × todos)
Book {ownerid = 1, todo = "implement relatable", todoid = 1, userid = 1, username = "stepcut"}
Book {ownerid = 2, todo = "whip it real good", todoid = 2, userid = 2, username = "ddssff"}
π_{#username, #todo} (σ_{#userid == #ownerid} (users × todos))
Book {todo = "implement relatable", username = "stepcut"}
Book {todo = "whip it real good", username = "ddssff"}
π_{#username, #todo} (σ_{#userid == 1} (σ_{#userid == #ownerid} (users × todos)))
Book {todo = "implement relatable", username = "stepcut"}
*Main>
```
