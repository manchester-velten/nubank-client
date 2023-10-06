{-# LANGUAGE DeriveGeneric #-}

module Nubank.Login where

import Data.Aeson
import GHC.Generics
import Data.Aeson.Casing
import GHC.Base
import Nubank.Discovery
import Nubank.Prolog
import Data.String
import Data.Time.Clock
import Nubank.HttpClient

data GrantType = Password deriving (Show, Eq)

instance ToJSON GrantType where
  toJSON Password = String "password"

data TokenType = Bearer deriving (Show, Eq)

instance FromJSON TokenType where
  parseJSON (String "bearer") = do return Bearer
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

passwordAuth :: LoginRequest -> IO LoginResponse
passwordAuth request = do
  loginUrl <- getLoginUrl
  postJSON loginUrl request
