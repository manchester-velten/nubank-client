{-# LANGUAGE DeriveGeneric #-}

module Discovery.Application (FAQDiscoveryURLs, ApplicationDiscoveryURLs, getUrls) where

import Prolog (URL)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpJSON)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, genericToJSON, genericParseJSON)
import Data.Aeson.Types (FromJSON(parseJSON))
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data FAQDiscoveryURLs = FAQDiscoveryURLs
  { ios  :: URL
  , android  :: URL
  , wp  :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON FAQDiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON FAQDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ApplicationDiscoveryURLs = ApplicationDiscoveryURLs
  { unloggedChallengePlatform  :: URL
  , scopes  :: URL
  , creation  :: URL
  , rosettaImages  :: URL
  , changePassword  :: URL
  , smokejumper  :: URL
  , block  :: URL
  , lift  :: URL
  , shardMappingId  :: URL
  , foundationTokens  :: URL
  , applicationStatusByTaxId  :: URL
  , forceResetPassword  :: URL
  , rosettaLocalization  :: URL
  , revokeToken  :: URL
  , userinfo  :: URL
  , resetPassword  :: URL
  , openTelemetryTracing  :: URL
  , unblock  :: URL
  , shardMappingCNPJ  :: URL
  , shardMappingCPF  :: URL
  , registerProspect  :: URL
  , engage  :: URL
  , accountRecoveryV2  :: URL
  , sendDataToETL  :: URL
  , creationWithCredentials  :: URL
  , magnitude  :: URL
  , revokeAll  :: URL
  , userUpdateCredential  :: URL
  , userHypermedia  :: URL
  , genCertificate  :: URL
  , deferredDeeplinkApplication  :: URL
  , emailVerify  :: URL
  , token  :: URL
  , accountRecovery  :: URL
  , startScreenV2  :: URL
  , scopesRemove  :: URL
  , approvedProducts  :: URL
  , startScreenV4  :: URL
  , adminRevokeAll  :: URL
  , faq :: FAQDiscoveryURLs
  , scopesAdd  :: URL
  , produceMarketingEvent  :: URL
  , registration  :: URL
  , globalServices  :: URL
  , startScreen  :: URL
  , forceResetPasswordWithoutRevokingToken  :: URL
  , userChangePassword  :: URL
  , rosettaLocalizationsByLocale  :: URL
  , remoteConfig  :: URL
  , fogWallDiscovery  :: URL
  , accountRecoveryToken  :: URL
  , userStatus  :: URL
  , engageAndCreateCredentials  :: URL
  , unloggedArea  :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON ApplicationDiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON ApplicationDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

discoveryUrl  :: URL
discoveryUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/app/discovery"

getUrls :: IO ApplicationDiscoveryURLs
getUrls = do
  request <- parseRequest discoveryUrl
  response <- httpJSON request
  return (getResponseBody response :: ApplicationDiscoveryURLs)
