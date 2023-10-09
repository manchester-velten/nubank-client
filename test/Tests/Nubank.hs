module Tests.Nubank (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Nubank

tests :: TestTree
tests = testGroup "Discovery"
  [ testGroup "Proxy URLs"
    [ testCase "Should load proxy URLs without errors" $ do
        _ <- getProxyUrls
        pure ()
    , testCase "Should load application proxy URLs without errors" $ do
        _ <- getApplicationProxyUrls
        pure ()
    ]
  ]
