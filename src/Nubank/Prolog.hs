module Nubank.Prolog (URL, newUUIDByteString, (<:>)) where

import System.Random
import qualified Data.UUID as UUID
import Data.ByteString

type URL = String

generateRandom :: (Random a) => IO a
generateRandom = fst . random <$> getStdGen

newUUIDByteString :: IO ByteString
newUUIDByteString = UUID.toASCIIBytes <$> generateRandom

(<:>) :: Monad m => a -> m [a] -> m [a]
(<:>) x xs = do
  xs' <- xs
  return (x : xs')
