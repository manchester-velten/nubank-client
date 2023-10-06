module Main (main) where

import Nubank

main :: IO ()
main = do
  urls <- getProxyUrls
  putStrLn "Proxy URLs:"
  print urls
  applicationURLs <- getApplicationProxyUrls
  putStrLn "Proxy Application URLs:"
  print applicationURLs
