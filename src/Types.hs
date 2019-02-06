{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.IO.Class
import Servant
--------------------------------------------------------------------------------

data FakeConn = FakeConn
data AppContext
  = AppContext
  { conn :: FakeConn -- in a real system this would be a pool
  }

newtype App a = App { runApp :: ReaderT AppContext Handler a }
  deriving (Functor, Applicative, Monad, MonadReader AppContext, MonadIO)
