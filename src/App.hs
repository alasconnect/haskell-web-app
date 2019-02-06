{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module App where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Proxy
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Servant
--------------------------------------------------------------------------------
import Api.Task
import Api.User
import Types
--------------------------------------------------------------------------------

type FullApi =
       UserApi
  :<|> TaskApi

fullApi :: Proxy FullApi
fullApi = Proxy

fullApiServer :: (MonadIO m, MonadReader AppContext m) => ServerT FullApi m
fullApiServer =
  usersServer :<|> tasksServer

handler :: App a -> Handler a
handler =
  flip runReaderT (AppContext FakeConn) . runApp

server :: Server FullApi
server =
  hoistServer fullApi handler fullApiServer

app :: Application
app =
  serve fullApi server

exec :: IO ()
exec =
  W.run 8080 app
