{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Models.Task where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Tagged (Tagged(..), untag)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import GHC.Generics
import Servant.API
--------------------------------------------------------------------------------
import Models.User (UserId)
--------------------------------------------------------------------------------

data TaskIdTag
type TaskId = Tagged TaskIdTag Int

instance ToHttpApiData TaskId where
  toUrlPiece = pack . show . untag
instance FromHttpApiData TaskId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . mkTaskId . fromInteger $ v
      Left e       -> Left . pack $ e

mkTaskId :: Int -> TaskId
mkTaskId = Tagged

data Task
  = Task
  { taskId :: TaskId
  , userId :: UserId
  , name   :: Text
  } deriving (Generic, Show)

instance ToJSON Task
instance FromJSON Task

mkTask :: UserId -> Text -> Task
mkTask = Task 0
