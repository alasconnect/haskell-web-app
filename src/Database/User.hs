{-# LANGUAGE FlexibleInstances #-}

module Database.User where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
--------------------------------------------------------------------------------
import Models.User
import Types
--------------------------------------------------------------------------------

getUsers :: MonadIO m => m [User]
getUsers = undefined

getUser :: MonadIO m => UserId -> m (Maybe User)
getUser = undefined

createUser :: MonadIO m => User -> m User
createUser = undefined

updateUser :: MonadIO m => User -> m ()
updateUser = undefined

deleteUser :: MonadIO m => UserId -> m ()
deleteUser = undefined
