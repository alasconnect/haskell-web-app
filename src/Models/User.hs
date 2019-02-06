{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Models.User where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Tagged (Tagged(..), untag)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import GHC.Generics
import Servant.API
--------------------------------------------------------------------------------

data UserIdTag
type UserId = Tagged UserIdTag Int

instance ToHttpApiData UserId where
  toUrlPiece = pack . show . untag
instance FromHttpApiData UserId where
  parseUrlPiece t =
    case decimal t of
      Right (v, _) -> Right . mkUserId . fromInteger $ v
      Left e       -> Left . pack $ e

mkUserId :: Int -> UserId
mkUserId = Tagged

data User
  = User
  { userId :: UserId
  , name   :: Text
  } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

mkUser :: Text -> User
mkUser = User 0
