module Nubank.HttpClient where

import Network.HTTP.Simple (RequestHeaders)
import System.Random (Random(random), getStdGen)
import Data.UUID (toASCIIBytes)
import Data.ByteString (ByteString)

getHeaders :: IO RequestHeaders
getHeaders = do
  correlationId <- getCorrelationId
  return [
      ("X-Correlation-Id", correlationId),
      ("User-Agent", "Nubank Haskell Client (nubank-client) - https://github.com/manchester-velten/nubank-client")
    ]
  where
    getCorrelationId :: IO ByteString
    getCorrelationId = toASCIIBytes .fst . random <$> getStdGen
