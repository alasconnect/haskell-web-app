module Domain.Task where

--------------------------------------------------------------------------------
import Models.Task (Task, TaskId)
import Models.User (UserId)
import Types
--------------------------------------------------------------------------------

getTasks :: Monad m
  => m [Task]
  -> m [Task]
getTasks f = f

getTask :: Monad m
  => (TaskId -> m (Maybe Task))
  -> TaskId
  -> m (Maybe Task)
getTask f tid = f tid

createTask :: Monad m
  => (Task -> m Task)
  -> Task
  -> m Task
createTask f t = f t

updateTask :: Monad m
  => (Task -> m ())
  -> Task
  -> m ()
updateTask f t = f t

deleteTask :: Monad m
  => (TaskId -> m ())
  -> TaskId
  -> m ()
deleteTask f tid = f tid

getUserTasks :: Monad m
  => (UserId -> m [Task])
  -> UserId
  -> m [Task]
getUserTasks f uid = f uid
