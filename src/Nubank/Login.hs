{-# LANGUAGE DeriveGeneric #-}

module Nubank.Login
  ( PasswordAuthenticationRequest (..)
  , PasswordAuthenticationResponse (..)
  , Login
  , Password
  , TokenType (..)
  , Links (..)
  , passwordAuth
  , passwordAuthSimple
  ) where

import Data.Aeson
import GHC.Generics
import Data.Aeson.Casing
import GHC.Base
import Nubank.Discovery
import Nubank.Prolog
import Data.String
import Data.Time.Clock
import Nubank.HttpClient

type Login = String

type Password = String

data TokenType = Bearer deriving (Show, Eq)

instance FromJSON TokenType where
  parseJSON (String "bearer") = do return Bearer
  parseJSON _ = empty

data RefreshToken = StringToken deriving (Show, Eq)

instance FromJSON RefreshToken where
  parseJSON (String "string token") = do return StringToken
  parseJSON _ = empty

data PasswordAuthenticationRequest = PasswordAuthenticationRequest
  { login        :: Login
  , password     :: Password
  , clientId     :: String
  , clientSecret :: String
  } deriving (Show, Eq)

instance ToJSON PasswordAuthenticationRequest where
  toJSON request =
    object [ "grant_type"    .= String "password"
           , "login"         .= login request
           , "password"      .= password request
           , "client_id"     .= clientId request
           , "client_secret" .= clientSecret request
           ]

newtype Link = Link
  { href :: URL
  } deriving (Eq, Generic)

instance Show Link where
  show l = show $ href l

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

data PasswordAuthenticationResponse = PasswordAuthenticationResponse
  { accessToken   :: String
  , tokenType     :: TokenType
  , _links        :: Links
  , refreshToken  :: RefreshToken
  , refreshBefore :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON PasswordAuthenticationResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

passwordAuth :: PasswordAuthenticationRequest -> IO PasswordAuthenticationResponse
passwordAuth request = do
  loginUrl <- getLoginUrl
  postJSON loginUrl request

passwordAuthSimple :: Login -> Password -> IO PasswordAuthenticationResponse
passwordAuthSimple usr pwd = do
  let request = PasswordAuthenticationRequest
       { login        = usr
       , password     = pwd
       , clientId     = "nubank-client-haskell"
       , clientSecret = "02ee8b31a70502a98454004481e1070ef28cc7c0cf8d623008904d607637556f"
       }
  passwordAuth request
