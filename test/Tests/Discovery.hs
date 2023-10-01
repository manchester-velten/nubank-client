module Tests.Discovery (tests) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import Nubank (getProxyUrls, getApplicationProxyUrls)
import Test.Tasty.Providers (TestTree)

tests :: TestTree
tests = testGroup "Discovery"
  [ testGroup "Proxy URLs"
    [ testCase "Should load proxy URLs without errors" $ do
        _ <- getProxyUrls
        return ()
    , testCase "Should load application proxy URLs without errors" $ do
        _ <- getApplicationProxyUrls
        return ()
    ]
  ]
