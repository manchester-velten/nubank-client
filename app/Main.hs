module Main (main) where

import Nubank.Auth.Login
import System.Environment

main :: IO ()
main = do
  [usr,pwd] <- getArgs
  loginResponse <- passwordAuthSimple usr pwd
  print loginResponse
