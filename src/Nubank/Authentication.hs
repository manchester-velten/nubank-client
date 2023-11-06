module Nubank.Authentication
  ( Credentials (..)
  , Authentication (..)
  , AuthenticationState (..)
  , getAuthenticationFromResponse
  , Authenticated (..)
  ) where

import Nubank.Login
import Data.Time

data Credentials = PasswordAuthentication
  { login    :: String
  , password :: Password
  } deriving (Show, Eq)

data Authentication = Authentication
  { token      :: String
  , links      :: Links
  , expiration :: UTCTime
  } deriving (Show, Eq)

data AuthenticationState = AuthenticationState
  { credentials    :: Credentials
  , authentication :: Authentication
  } deriving (Show, Eq)

getAuthenticationFromResponse :: PasswordAuthenticationResponse -> Authentication
getAuthenticationFromResponse response =
  Authentication
    { token      = accessToken response
    , links      = _links response
    , expiration = refreshBefore response
    }

authenticatedOperation :: Authenticated a -> AuthenticationState -> IO (a, AuthenticationState)
authenticatedOperation (Authenticated op) = op

newtype Authenticated a = Authenticated (AuthenticationState -> IO (a, AuthenticationState))

instance Functor Authenticated where
  fmap f op = Authenticated (\s -> do
    (x, s') <- authenticatedOperation op s
    return (f x, s'))

instance Applicative Authenticated where
  pure x = Authenticated (\s ->
    do return (x, s))
  xf <*> xa = Authenticated (\s -> do
    (f, s')  <- authenticatedOperation xf s
    (x, s'') <- authenticatedOperation xa s'
    return (f x, s''))

instance Monad Authenticated where
  x >>= f = Authenticated (\s -> do
    (x', s') <- authenticatedOperation x s
    authenticatedOperation (f x') s')
