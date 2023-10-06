{-# LANGUAGE DeriveGeneric #-}

module Nubank.Discovery (DiscoveryURLs, getProxyUrls, getLoginUrl) where
import Nubank.Prolog
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Nubank.HttpClient

data DiscoveryURLs = DiscoveryURLs
  { companySocialInviteBySlug      :: URL
  , registerProspectSavingsWeb     :: URL
  , commonXp                       :: URL
  , pusherAuthChannel              :: URL
  , applicationStatusByTaxId       :: URL
  , resetPassword                  :: URL
  , registerProspectUltravioletWeb :: URL
  , businessCardWaitlist           :: URL
  , registerProspect               :: URL
  , registerProspectGlobalWeb      :: URL
  , registerProspectC              :: URL
  , requestPasswordReset           :: URL
  , authGenCertificates            :: URL
  , login                          :: URL
  , emailVerify                    :: URL
  , registerProspectCompany        :: URL
  , getCustomerSessions            :: URL
  , authDeviceResendCode           :: URL
  , msat                           :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON DiscoveryURLs where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON DiscoveryURLs where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

discoveryUrl :: URL
discoveryUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/discovery"

getProxyUrls :: IO DiscoveryURLs
getProxyUrls = getJSON discoveryUrl

getLoginUrl :: IO URL
getLoginUrl = login <$> getProxyUrls
