module Nubank.HttpClient (getJSON, postJSON) where

import Network.HTTP.Simple
import Data.Aeson
import Nubank.Prolog

getHeaders :: IO RequestHeaders
getHeaders = do
  correlationId <- getUUIDString
  return
    [ ("X-Correlation-Id", correlationId)
    , ("User-Agent", "Nubank Haskell Client (nubank-client) - https://github.com/manchester-velten/nubank-client")
    ]

getJSON :: (FromJSON a) => URL -> IO a
getJSON url = do
  headers <- getHeaders
  request <- setRequestHeaders headers <$> parseRequest url
  response <- httpJSON request
  return (getResponseBody response)

postJSON :: (ToJSON a, FromJSON b) => URL -> a -> IO b
postJSON url body = do
  headers  <-  getHeaders
  request  <-  setRequestMethod "POST"
            .  setRequestHeaders headers
            .  setRequestBodyJSON body
           <$> parseRequest url
  response <-  httpJSON request
  return (getResponseBody response)
