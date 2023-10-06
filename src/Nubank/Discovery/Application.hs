{-# LANGUAGE DeriveGeneric #-}

module Nubank.Discovery.Application
  ( FAQDiscoveryURLs
  , ApplicationDiscoveryURLs
  , getApplicationProxyUrls
  ) where

import Nubank.Prolog
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Nubank.HttpClient

data FAQDiscoveryURLs = FAQDiscoveryURLs
  { ios      :: URL
  , android  :: URL
  , wp       :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON FAQDiscoveryURLs where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON FAQDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data ApplicationDiscoveryURLs = ApplicationDiscoveryURLs
  { unloggedChallengePlatform               :: URL
  , scopes                                  :: URL
  , creation                                :: URL
  , rosettaImages                           :: URL
  , changePassword                          :: URL
  , smokejumper                             :: URL
  , block                                   :: URL
  , lift                                    :: URL
  , shardMappingId                          :: URL
  , foundationTokens                        :: URL
  , applicationStatusByTaxId                :: URL
  , forceResetPassword                      :: URL
  , rosettaLocalization                     :: URL
  , revokeToken                             :: URL
  , userinfo                                :: URL
  , resetPassword                           :: URL
  , openTelemetryTracing                    :: URL
  , unblock                                 :: URL
  , shardMappingCnpj                        :: URL
  , shardMappingCpf                         :: URL
  , registerProspect                        :: URL
  , engage                                  :: URL
  , accountRecoveryV2                       :: URL
  , sendDataToEtl                           :: URL
  , creationWithCredentials                 :: URL
  , magnitude                               :: URL
  , revokeAll                               :: URL
  , userUpdateCredential                    :: URL
  , userHypermedia                          :: URL
  , genCertificate                          :: URL
  , deferredDeeplinkApplication             :: URL
  , emailVerify                             :: URL
  , token                                   :: URL
  , accountRecovery                         :: URL
  , startScreenV2                           :: URL
  , scopesRemove                            :: URL
  , approvedProducts                        :: URL
  , startScreenV4                           :: URL
  , adminRevokeAll                          :: URL
  , faq                                     :: FAQDiscoveryURLs
  , scopesAdd                               :: URL
  , produceMarketingEvent                   :: URL
  , registration                            :: URL
  , globalServices                          :: URL
  , startScreen                             :: URL
  , forceResetPasswordWithoutRevokingToken  :: URL
  , userChangePassword                      :: URL
  , rosettaLocalizationsByLocale            :: URL
  , remoteConfig                            :: URL
  , fogWallDiscovery                        :: URL
  , accountRecoveryToken                    :: URL
  , userStatus                              :: URL
  , engageAndCreateCredentials              :: URL
  , unloggedArea                            :: URL
  } deriving (Show, Eq, Generic)

instance ToJSON ApplicationDiscoveryURLs where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance FromJSON ApplicationDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

discoveryUrl  :: URL
discoveryUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/app/discovery"

getApplicationProxyUrls :: IO ApplicationDiscoveryURLs
getApplicationProxyUrls = getJSON discoveryUrl
