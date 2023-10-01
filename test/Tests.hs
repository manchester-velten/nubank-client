module Tests (tests) where

import Test.Tasty (testGroup)
import qualified Tests.Discovery as Discovery
import Test.Tasty.Providers (TestTree)

tests :: TestTree
tests =
  testGroup "All tests"
    [ Discovery.tests
    ]
