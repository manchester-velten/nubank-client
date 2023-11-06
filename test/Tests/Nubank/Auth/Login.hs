module Tests.Nubank.Auth.Login (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson
import Nubank.Auth.Login

tests :: TestTree
tests = testGroup "Serialization tests"
  [ testCase "Should serialize PasswordAuthRequest" $ do
      let request = PasswordAuthRequest
           { login        = "99999999999"
           , password     = "Xj;BW4d=0G5[7Jt"
           , clientId     = "nubank-client-haskell"
           , clientSecret = "02ee8b31a70502a98454004481e1070ef28cc7c0cf8d623008904d607637556f"
           }
      let json = encode request
      json @?= "{\"client_id\":\"nubank-client-haskell\",\"client_secret\":\"02ee8b31a70502a98454004481e1070ef28cc7c0cf8d623008904d607637556f\",\"grant_type\":\"password\",\"login\":\"99999999999\",\"password\":\"Xj;BW4d=0G5[7Jt\"}"
  ]
