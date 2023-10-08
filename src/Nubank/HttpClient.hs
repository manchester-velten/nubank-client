module Nubank.HttpClient (getJSON, postJSON) where

import Network.HTTP.Simple
import Data.Aeson
import Nubank.Prolog

defaultHeaders :: IO RequestHeaders
defaultHeaders = do
  correlationId <- getUUIDString
  return
    [ ("X-Correlation-Id", correlationId)
    , ("User-Agent", "Nubank Haskell Client (nubank-client) - https://github.com/manchester-velten/nubank-client")
    ]

defaultPostHeaders :: IO RequestHeaders
defaultPostHeaders = ("Content-Type", "application/json") <:> defaultHeaders

getJSON :: (FromJSON a) => URL -> IO a
getJSON url = do
  headers <- defaultPostHeaders
  request <- setRequestHeaders headers <$> parseRequest url
  response <- httpJSON request
  return (getResponseBody response)

postJSON :: (ToJSON a, FromJSON b) => URL -> a -> IO b
postJSON url body = do
  headers  <-  defaultPostHeaders
  request  <-  setRequestMethod "POST"
            .  setRequestHeaders headers
            .  setRequestBodyJSON body
           <$> parseRequest url
  response <-  httpJSON request
  return (getResponseBody response)
