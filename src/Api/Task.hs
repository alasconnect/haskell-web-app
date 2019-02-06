{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Task where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Data.Proxy
import Servant
--------------------------------------------------------------------------------
import qualified Database.Task as DT
import Domain.Task
import Models.Task (Task, TaskId)
--------------------------------------------------------------------------------

type GetTasksApi
  = Get '[JSON] [Task]

type GetTaskApi
  =  Capture "task_id" TaskId
  :> Get '[JSON] (Maybe Task)

type CreateTaskApi
  =  ReqBody '[JSON] Task
  :> Post '[JSON] Task

type UpdateTaskApi
  =  ReqBody '[JSON] Task
  :> Put '[JSON] NoContent

type DeleteTaskApi
  =  Capture "task_id" TaskId
  :> Delete '[JSON] NoContent

type TaskApi
  =  "api"
  :> "v1"
  :> "tasks"
  :> (    GetTasksApi
     :<|> GetTaskApi
     :<|> CreateTaskApi
     :<|> UpdateTaskApi
     :<|> DeleteTaskApi
     )

tasksApi :: Proxy TaskApi
tasksApi = Proxy

tasksServer :: MonadIO m => ServerT TaskApi m
tasksServer =
       tasksGet
  :<|> taskGet
  :<|> taskCreate
  :<|> taskUpdate
  :<|> taskDelete

tasksGet :: MonadIO m => m [Task]
tasksGet =
  getTasks DT.getTasks

taskGet :: MonadIO m => TaskId -> m (Maybe Task)
taskGet tid =
  getTask DT.getTask tid

taskCreate :: MonadIO m => Task -> m Task
taskCreate t =
  createTask DT.createTask t

taskUpdate :: MonadIO m => Task -> m NoContent
taskUpdate t = do
  updateTask DT.updateTask t
  return NoContent

taskDelete :: MonadIO m => TaskId -> m NoContent
taskDelete tid = do
  deleteTask DT.deleteTask tid
  return NoContent
