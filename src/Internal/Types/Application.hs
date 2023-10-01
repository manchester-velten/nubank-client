{-# LANGUAGE DeriveGeneric #-}
module Internal.Types.Application where

import Internal.Types (FAQDiscoveryURLs)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), FromJSON, genericToJSON, genericParseJSON)
import Data.Aeson.Types (FromJSON(parseJSON))
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data AppDiscoveryURLs = AppDiscoveryURLs
  { unloggedChallengePlatform :: String
  , scopes :: String
  , creation :: String
  , rosettaImages :: String
  , changePassword :: String
  , smokejumper :: String
  , block :: String
  , lift :: String
  , shardMappingId :: String
  , foundationTokens :: String
  , applicationStatusByTaxId :: String
  , forceResetPassword :: String
  , rosettaLocalization :: String
  , revokeToken :: String
  , userinfo :: String
  , resetPassword :: String
  , openTelemetryTracing :: String
  , unblock :: String
  , shardMappingCNPJ :: String
  , shardMappingCPF :: String
  , registerProspect :: String
  , engage :: String
  , accountRecoveryV2 :: String
  , sendDataToETL :: String
  , creationWithCredentials :: String
  , magnitude :: String
  , revokeAll :: String
  , userUpdateCredential :: String
  , userHypermedia :: String
  , genCertificate :: String
  , deferredDeeplinkApplication :: String
  , emailVerify :: String
  , token :: String
  , accountRecovery :: String
  , startScreenV2 :: String
  , scopesRemove :: String
  , approvedProducts :: String
  , startScreenV4 :: String
  , adminRevokeAll :: String
  , faq :: FAQDiscoveryURLs
  , scopesAdd :: String
  , produceMarketingEvent :: String
  , registration :: String
  , globalServices :: String
  , startScreen :: String
  , forceResetPasswordWithoutRevokingToken :: String
  , userChangePassword :: String
  , rosettaLocalizationsByLocale :: String
  , remoteConfig :: String
  , fogWallDiscovery :: String
  , accountRecoveryToken :: String
  , userStatus :: String
  , engageAndCreateCredentials :: String
  , unloggedArea :: String } deriving (Show, Eq, Generic)

instance ToJSON AppDiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON AppDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
