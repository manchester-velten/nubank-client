module Main (main) where

import Test.Tasty (defaultMain)
import Tests (tests)

main :: IO ()
main = defaultMain tests
