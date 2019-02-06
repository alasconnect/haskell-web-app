{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import qualified Database.User as DU
import qualified Database.Task as DT
import Domain.Task
import Domain.User
import Models.Task (Task)
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

type GetUsersApi
  = Get '[JSON] [User]

type GetUserApi
  =  Capture "user_id" UserId
  :> Get '[JSON] (Maybe User)

type CreateUserApi
  =  ReqBody '[JSON] User
  :> Post '[JSON] User

type UpdateUserApi
  =  ReqBody '[JSON] User
  :> Put '[JSON] NoContent

type DeleteUserApi
  =  Capture "user_id" UserId
  :> Delete '[JSON] NoContent

type GetUserTasksApi
  =  Capture "user_id" UserId
  :> Get '[JSON] [Task]

type GetUserAndTasksApi
  =  "tasks"
  :> Capture "user_id" UserId
  :> Get '[JSON] (Maybe User, [Task])

type UserApi
  =  "api"
  :> "v1"
  :> "users"
  :> (    GetUsersApi
     :<|> GetUserApi
     :<|> CreateUserApi
     :<|> UpdateUserApi
     :<|> DeleteUserApi
     :<|> GetUserTasksApi
     :<|> GetUserAndTasksApi
     )

usersApi :: Proxy UserApi
usersApi = Proxy

usersServer :: (MonadIO m, MonadReader AppContext m) => ServerT UserApi m
usersServer =
       usersGet
  :<|> userGet
  :<|> userCreate
  :<|> userUpdate
  :<|> userDelete
  :<|> userTasks
  :<|> userAndTasks

usersGet :: MonadIO m => m [User]
usersGet =
  getUsers DU.getUsers

userGet :: MonadIO m => UserId -> m (Maybe User)
userGet uid =
  getUser DU.getUser uid

userCreate :: MonadIO m => User -> m User
userCreate u =
  createUser DU.createUser u

userUpdate :: MonadIO m => User -> m NoContent
userUpdate u = do
  updateUser DU.updateUser u
  return NoContent

userDelete :: MonadIO m => UserId -> m NoContent
userDelete uid = do
  deleteUser DU.deleteUser uid
  return NoContent

userTasks :: MonadIO m => UserId -> m [Task]
userTasks uid =
  getUserTasks DT.getUserTasks uid

userAndTasks :: (MonadIO m, MonadReader AppContext m)
  => UserId -> m (Maybe User, [Task])
userAndTasks uid = do
  ctx <- ask
  u <- getUser DU.getUser uid
  ts <- getUserTasks DT.getUserTasks uid
  return (u, ts)
