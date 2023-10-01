module Main where

import Nubank ( getProxyUrls, getApplicationProxyUrls )

main :: IO ()
main = do
  urls <- getProxyUrls
  putStrLn "Proxy URLs:"
  print urls
  applicationURLs <- getApplicationProxyUrls
  putStrLn "Proxy Application URLs:"
  print applicationURLs
