module Nubank.Prolog (URL, getUUIDString, (<:>)) where

import System.Random
import Data.UUID
import Data.ByteString

type URL = String

getUUIDString :: IO ByteString
getUUIDString = toASCIIBytes . fst . random <$> getStdGen

(<:>) :: Monad m => a -> m [a] -> m [a]
(<:>) x xs = do
  xs' <- xs
  return (x : xs')
