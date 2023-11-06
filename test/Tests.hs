module Tests (tests) where

import Test.Tasty
import qualified Tests.Nubank as Nubank
import qualified Tests.Nubank.Auth.Login as Login

tests :: TestTree
tests =
  testGroup "All tests"
    [ Nubank.tests
    , Login.tests
    ]
