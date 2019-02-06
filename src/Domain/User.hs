module Domain.User where

--------------------------------------------------------------------------------
import Models.User (User, UserId)
import Types
--------------------------------------------------------------------------------

getUsers :: Monad m
  => m [User]
  -> m [User]
getUsers f = f

getUser :: Monad m
  => (UserId -> m (Maybe User))
  -> UserId
  -> m (Maybe User)
getUser f uid = f uid

createUser :: Monad m
  => (User -> m User)
  -> User
  -> m User
createUser f u = f u

updateUser :: Monad m
  => (User -> m ())
  -> User
  -> m ()
updateUser f u = f u

deleteUser :: Monad m
  => (UserId -> m ())
  -> UserId
  -> m ()
deleteUser f uid = f uid
