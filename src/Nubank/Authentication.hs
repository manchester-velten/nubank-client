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

applyAuthentication :: Authenticated a -> AuthenticationState -> IO (a, AuthenticationState)
applyAuthentication (Authenticated op) = op

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

autoRenewAuthenticationState :: AuthenticationState -> IO AuthenticationState
autoRenewAuthenticationState s = do
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
    s'       <- autoRenewAuthenticationState s
    (x, s'') <- applyAuthentication op s'
    return (f x, s''))

instance Applicative Authenticated where
  pure x = Authenticated (\s -> do
    s' <- autoRenewAuthenticationState s
    return (x, s'))
  xf <*> xa = Authenticated (\s -> do
    s'        <- autoRenewAuthenticationState s
    (f, s'')  <- applyAuthentication xf s'
    (x, s''') <- applyAuthentication xa s''
    return (f x, s'''))

instance Monad Authenticated where
  x >>= f = Authenticated (\s -> do
    s'        <- autoRenewAuthenticationState s
    (x', s'') <- applyAuthentication x s'
    applyAuthentication (f x') s'')

authenticate :: AuthenticationMethod -> Authenticated AuthenticationState
authenticate method = Authenticated (\s -> do
  state <- getAuthenticationState method
  return (s, state))
