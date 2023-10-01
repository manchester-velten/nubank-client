{-# LANGUAGE DeriveGeneric #-}

module Nubank.Login where

import Data.Aeson (ToJSON (toJSON), genericToJSON, FromJSON (parseJSON), genericParseJSON, Value (String))
import GHC.Generics (Generic)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import GHC.Base (empty)
import Nubank (URL)
import Data.String (IsString, fromString)
import Data.Time.Clock

data GrantType = Password deriving (Show, Eq)

instance ToJSON GrantType where
  toJSON Password = "password"

data TokenType = Bearer deriving (Show, Eq)

instance FromJSON TokenType where
  parseJSON (String "token_type") = do return Bearer
  parseJSON _ = empty

data LoginRequest = LoginRequest
  { grantType    :: GrantType
  , login        :: String
  , password     :: String
  , clientId     :: String
  , clientSecret :: String
  } deriving (Show, Eq, Generic)

instance ToJSON LoginRequest where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

newtype Link = Link
  { href :: URL
  } deriving (Show, Eq, Generic)

instance FromJSON Link where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

instance IsString Link where
  fromString string = Link { href = string }

data Links = Links
  { revokeToken        :: Link
  , revokeAll          :: Link
  , accountEmergency   :: Link
  , billEmergency      :: Link
  , blockPhysicalCards :: Link
  , blockVirtualCards  :: Link
  , listCards          :: Link
  } deriving (Show, Eq, Generic)

instance FromJSON Links where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data LoginResponse = LoginResponse
  { accessToken   :: String
  , tokenType     :: TokenType
  , links         :: Links
  , refreshToken  :: String
  , refreshBefore :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase
