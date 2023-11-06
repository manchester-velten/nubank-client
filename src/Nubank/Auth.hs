module Nubank.Auth
  ( AuthMethod (..)
  , Authentication (..)
  , AuthState (..)
  , Authenticated (..)
  , authenticate
  ) where

import Data.Time
import Nubank.Auth.Login

data AuthMethod = LoginAndPassword Login Password
  deriving (Show, Eq)

data Authentication = Authentication
  { token      :: String
  , links      :: Links
  , expiration :: UTCTime
  } deriving (Show, Eq)

data AuthState = AuthState
  { authenticationMethod :: AuthMethod
  , authentication       :: Authentication
  } deriving (Show, Eq)

applyAuthentication :: Authenticated a -> AuthState -> IO (a, AuthState)
applyAuthentication (Authenticated op) = op

getAuthenticationState :: AuthMethod -> IO AuthState
getAuthenticationState (LoginAndPassword l p) = do
  authResponse <- passwordAuthSimple l p
  let auth = Authentication
        { token      = accessToken authResponse
        , links      = _links authResponse
        , expiration = refreshBefore authResponse
        }
  let state = AuthState
        { authenticationMethod = LoginAndPassword l p
        , authentication       = auth }
  return state

autoRenewAuthenticationState :: AuthState -> IO AuthState
autoRenewAuthenticationState s = do
  currentTime <- getCurrentTime
  let expirationTime = expiration (authentication s)
  let diff           = nominalDiffTimeToSeconds $ diffUTCTime expirationTime currentTime
  let method         = authenticationMethod s
  if diff <= 10
    then getAuthenticationState method
    else return s

newtype Authenticated a = Authenticated (AuthState -> IO (a, AuthState))

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

authenticate :: AuthMethod -> Authenticated AuthState
authenticate method = Authenticated (\s -> do
  state <- getAuthenticationState method
  return (s, state))
