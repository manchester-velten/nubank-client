{-# LANGUAGE OverloadedStrings #-}

module Internal.Discovery where

import Internal.Types (DiscoveryURLs)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpJSON)
import Internal.Types.Application (AppDiscoveryURLs)

discoveryUrl :: String
discoveryUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/discovery"

discoveryAppUrl :: String
discoveryAppUrl = "https://prod-s0-webapp-proxy.nubank.com.br/api/app/discovery"

getProxyUrls :: IO DiscoveryURLs
getProxyUrls = do
  request <- parseRequest discoveryUrl
  response <- httpJSON request
  return (getResponseBody response :: DiscoveryURLs)

getAppProxyUrls :: IO AppDiscoveryURLs
getAppProxyUrls = do
  request <- parseRequest discoveryUrl
  response <- httpJSON request
  return (getResponseBody response :: AppDiscoveryURLs)
