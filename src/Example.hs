{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Example where

--------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
--------------------------------------------------------------------------------

data Counter r where
  Val :: Counter Int
  Inc :: Int -> Counter ()
  Dec :: Int -> Counter ()

val :: Member Counter effs => Eff effs Int
val = send Val

inc :: Member Counter effs => Int -> Eff effs ()
inc = send . Inc

dec :: Member Counter effs => Int -> Eff effs ()
dec = send . Dec

runCounterInMemory :: Int -> Eff (Counter ': effs) ~> Eff effs
runCounterInMemory i = evalState i . go
  where
    go :: Eff (Counter ': effs) ~> Eff (State Int ': effs)
    go = reinterpret $ \case
      Val   -> get
      Inc v -> modify (\a -> a + v)
      Dec v -> modify (\a -> a - v)

data User = User Text Text deriving Show

data Mapper r where
  UserAll :: Mapper [User]
  UserGet :: Text -> Mapper (Maybe User)
  UserAdd :: Text -> User -> Mapper ()
  UserDel :: Text -> Mapper ()

userAll :: Member Mapper effs => Eff effs [User]
userAll = send UserAll

userGet :: Member Mapper effs => Text -> Eff effs (Maybe User)
userGet = send . UserGet

userAdd :: Member Mapper effs => Text -> User -> Eff effs ()
userAdd k v = send $ UserAdd k v

userDel :: Member Mapper effs => Text -> Eff effs ()
userDel = send . UserDel

type Vdb = Map Text User

runMapperInMemory :: Vdb -> Eff (Mapper ': effs) ~> Eff effs
runMapperInMemory vdb = evalState vdb . go
  where
    go :: Eff (Mapper ': effs) ~> Eff (State Vdb ': effs)
    go = reinterpret $ \case
      UserAll     -> get >>= \db -> return . Map.elems $ (db :: Vdb)
      UserGet k   -> get >>= return . Map.lookup k
      UserAdd k v -> modify (\db -> Map.insert k v db :: Vdb)
      UserDel k   -> modify (\db -> Map.delete k db :: Vdb)

counterApp :: Members '[Counter] effs => Eff effs Int
counterApp = do
  inc 10
  dec 3
  inc 5
  dec 15
  dec 1
  val

mapperApp :: Members '[IO, Mapper] effs => Eff effs [User]
mapperApp = do
  userAdd "luke" (User "Luke" "Cage")
  userAdd "jess" (User "Jessica" "Jones")
  userAdd "matt" (User "Matt" "Murdock")
  luke <- userGet "luke"
  send $ print luke
  userDel "jess"
  jess <- userGet "jess"
  send $ print jess
  userAll

allApp :: Members '[IO, Mapper, Counter] effs => Eff effs (Int, [User])
allApp = do
  userAdd "luke" (User "Luke" "Cage")
  userAdd "jess" (User "Jessica" "Jones")
  inc 10
  userAdd "matt" (User "Matt" "Murdock")
  luke <- userGet "luke"
  inc 20
  dec 5
  send $ print luke
  userDel "jess"
  jess <- userGet "jess"
  dec 13
  send $ print jess
  us <- userAll
  n <- val
  return (n, us)

exec :: IO ()
exec = do
  -- counter
  putStrLn "--------------------"
  putStrLn "counter"
  putStrLn "--------------------"
  let v = run . runCounterInMemory 0 $ counterApp
  putStrLn $ show v

  -- mapper
  putStrLn "--------------------"
  putStrLn "mapper"
  putStrLn "--------------------"
  m <- runM . runMapperInMemory Map.empty $ mapperApp
  putStrLn $ show m

  -- combined
  putStrLn "--------------------"
  putStrLn "combined"
  putStrLn "--------------------"
  (n, us) <- runM . runMapperInMemory Map.empty . runCounterInMemory 0 $ allApp
  print n
  print us
