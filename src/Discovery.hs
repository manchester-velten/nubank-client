{-# LANGUAGE DeriveGeneric #-}

module Discovery (DiscoveryURLs, getDiscoveryUrls) where

import Prolog (URL)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpJSON)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, genericToJSON, genericParseJSON)
import Data.Aeson.Types (FromJSON(parseJSON))
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data DiscoveryURLs = DiscoveryURLs
  { companySocialInviteBySlug :: URL
  , registerProspectSavingsWeb :: URL
  , commonXP :: URL
  , pusherAuthChannel :: URL
  , applicationStatusByTaxId :: URL
  , resetPassword :: URL
  , registerProspectUltravioletWeb :: URL
  , businessCardWaitlist :: URL
  , registerProspect :: URL
  , registerProspectGlobalWeb :: URL
  , registerProspectC :: URL
  , requestPasswordReset :: URL
  , authGenCertificates :: URL
  , login :: URL
  , emailVerify :: URL
  , registerProspectCompany :: URL
  , getCustomerSessions :: URL
  , authDeviceResendCode :: URL
  , msat :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON DiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON DiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

discoveryUrl :: URL
discoveryUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/discovery"

getDiscoveryUrls :: IO DiscoveryURLs
getDiscoveryUrls = do
  request <- parseRequest discoveryUrl
  response <- httpJSON request
  return (getResponseBody response :: DiscoveryURLs)
