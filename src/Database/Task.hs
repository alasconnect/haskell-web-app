{-# LANGUAGE FlexibleContexts #-}

module Database.Task where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
--------------------------------------------------------------------------------
import Models.Task
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

getTasks :: MonadIO m => m [Task]
getTasks = undefined

getTask :: MonadIO m => TaskId -> m (Maybe Task)
getTask = undefined

createTask :: MonadIO m => Task -> m Task
createTask = undefined

updateTask :: MonadIO m => Task -> m ()
updateTask = undefined

deleteTask :: MonadIO m => TaskId -> m ()
deleteTask = undefined

getUserTasks :: MonadIO m => UserId -> m [Task]
getUserTasks = undefined
