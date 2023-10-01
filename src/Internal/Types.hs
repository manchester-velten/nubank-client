{-# LANGUAGE DeriveGeneric #-}

module Internal.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), genericToJSON, FromJSON (parseJSON), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data DiscoveryURLs = DiscoveryURLs
  { companySocialInviteBySlug :: String
  , registerProspectSavingsWeb :: String
  , commonXP :: String
  , pusherAuthChannel :: String
  , applicationStatusByTaxId :: String
  , resetPassword :: String
  , registerProspectUltravioletWeb :: String
  , businessCardWaitlist :: String
  , registerProspect :: String
  , registerProspectGlobalWeb :: String
  , registerProspectC :: String
  , requestPasswordReset :: String
  , authGenCertificates :: String
  , login :: String
  , emailVerify :: String
  , registerProspectCompany :: String
  , getCustomerSessions :: String
  , authDeviceResendCode :: String
  , msat :: String } deriving (Show, Eq, Generic)

instance ToJSON DiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON DiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data FAQDiscoveryURLs = FAQDiscoveryURLs
  { ios :: String
  , android :: String
  , wp :: String} deriving (Show, Eq, Generic)

instance ToJSON FAQDiscoveryURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON FAQDiscoveryURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
