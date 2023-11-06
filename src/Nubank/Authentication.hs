module Nubank.Authentication
  ( AuthenticationMethod (..)
  , Authentication (..)
  , AuthenticationState (..)
  , Authenticated (..)
  , authenticate
  ) where

import Data.Time
import Nubank.Login

data AuthenticationMethod = LoginAndPassword Login Password
  deriving (Show, Eq)

data Authentication = Authentication
  { token      :: String
  , links      :: Links
  , expiration :: UTCTime
  } deriving (Show, Eq)

data AuthenticationState = AuthenticationState
  { authenticationMethod :: AuthenticationMethod
  , authentication       :: Authentication
  } deriving (Show, Eq)

applyAuth :: Authenticated a -> AuthenticationState -> IO (a, AuthenticationState)
applyAuth (Authenticated op) = op

getAuthenticationState :: AuthenticationMethod -> IO AuthenticationState
getAuthenticationState (LoginAndPassword l p) = do
  authResponse <- passwordAuthSimple l p
  let auth = Authentication
        { token      = accessToken authResponse
        , links      = _links authResponse
        , expiration = refreshBefore authResponse
        }
  let state = AuthenticationState
        { authenticationMethod = LoginAndPassword l p
        , authentication       = auth }
  return state

autoRenewAuthentication :: AuthenticationState -> IO AuthenticationState
autoRenewAuthentication s = do
  currentTime <- getCurrentTime
  let expirationTime = expiration (authentication s)
  let diff           = nominalDiffTimeToSeconds $ diffUTCTime expirationTime currentTime
  let method         = authenticationMethod s
  if diff <= 10
    then getAuthenticationState method
    else return s

newtype Authenticated a = Authenticated (AuthenticationState -> IO (a, AuthenticationState))

instance Functor Authenticated where
  fmap f op = Authenticated (\s -> do
    s' <- autoRenewAuthentication s
    (x, s'') <- applyAuth op s'
    return (f x, s''))

instance Applicative Authenticated where
  pure x = Authenticated (\s -> do
    s' <- autoRenewAuthentication s
    return (x, s'))
  xf <*> xa = Authenticated (\s -> do
    s' <- autoRenewAuthentication s
    (f, s'')  <- applyAuth xf s'
    (x, s''') <- applyAuth xa s''
    return (f x, s'''))

instance Monad Authenticated where
  x >>= f = Authenticated (\s -> do
    s' <- autoRenewAuthentication s
    (x', s'') <- applyAuth x s'
    applyAuth (f x') s'')

authenticate :: AuthenticationMethod -> Authenticated AuthenticationState
authenticate method = Authenticated (\s -> do
  state <- getAuthenticationState method
  return (s, state))
