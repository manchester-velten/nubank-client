module Tests.Discovery (tests) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import Nubank (getProxyUrls, getApplicationProxyUrls)
import Test.Tasty.Providers (TestTree)

tests :: TestTree
tests =
  testGroup "Discovery" [
    testGroup "Proxy URLs" [
      testCase "Load Proxy URLs" $ do
        _ <- getProxyUrls
        return (),
      testCase "Application Proxy URLs" $ do
        _ <- getApplicationProxyUrls
        return ()
    ]
  ]
