module Nubank.HttpClient (getJSON) where

import Network.HTTP.Simple (RequestHeaders, setRequestHeaders, httpJSON, getResponseBody)
import System.Random (Random(random), getStdGen)
import Data.UUID (toASCIIBytes)
import Data.ByteString (ByteString)
import Data.Aeson (FromJSON)
import Nubank.Prolog (URL)
import Network.HTTP.Client.Conduit (parseRequest)

getHeaders :: IO RequestHeaders
getHeaders = do
  correlationId <- getCorrelationId
  return
    [ ("X-Correlation-Id", correlationId)
    , ("User-Agent", "Nubank Haskell Client (nubank-client) - https://github.com/manchester-velten/nubank-client")
    ]
  where
    getCorrelationId :: IO ByteString
    getCorrelationId = toASCIIBytes .fst . random <$> getStdGen

getJSON :: (FromJSON a) => URL -> IO a
getJSON url = do
  headers <- getHeaders
  request <- setRequestHeaders headers <$> parseRequest url
  response <- httpJSON request
  return (getResponseBody response)
