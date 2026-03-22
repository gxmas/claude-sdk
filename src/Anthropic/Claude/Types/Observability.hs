{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Observability
Description : Observability event types for monitoring and tracing
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Callback-based observability layer for the Claude SDK. Users provide
an 'EventHandler' to observe request/response lifecycle events.

Zero-cost when unused: if no handler is set ('Nothing'), no event
data is constructed.
-}
module Anthropic.Claude.Types.Observability
  ( -- * Event Types
    APIEvent(..)
  , HttpRequestEvent(..)
  , HttpResponseEvent(..)
  , RetryEvent(..)

    -- * Event Handler
  , EventHandler

    -- * Combinators
  , noOpHandler
  , combineHandlers

    -- * Internal
  , emitEvent
  ) where

import Anthropic.Claude.Types.Common (RateLimitInfo)
import Anthropic.Claude.Types.Core (RequestId)
import Anthropic.Claude.Types.Error (APIError)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime)

-- | An event handler receives 'APIEvent' values at lifecycle points.
type EventHandler = APIEvent -> IO ()

-- | SDK lifecycle events emitted during API calls.
data APIEvent
  = HttpRequest  HttpRequestEvent
  | HttpResponse HttpResponseEvent
  | Retry        RetryEvent
  deriving (Eq, Show)

-- | Emitted before an HTTP request is sent.
data HttpRequestEvent = HttpRequestEvent
  { reqMethod    :: Text
  , reqPath      :: Text
  , reqTimestamp :: UTCTime
  } deriving (Eq, Show)

-- | Emitted after an HTTP response is received.
data HttpResponseEvent = HttpResponseEvent
  { respStatusCode    :: Int
  , respDuration      :: NominalDiffTime
  , respRequestId     :: Maybe RequestId
  , respRateLimitInfo :: Maybe RateLimitInfo
  } deriving (Eq, Show)

-- | Emitted before a retry delay.
data RetryEvent = RetryEvent
  { retryEvtError       :: APIError
  , retryEvtAttempt     :: Int             -- ^ 1-based attempt number
  , retryEvtMaxAttempts :: Int
  , retryEvtBackoff     :: NominalDiffTime -- ^ Delay before next attempt
  } deriving (Eq, Show)

-- | A handler that does nothing. Useful as a default.
noOpHandler :: EventHandler
noOpHandler _ = pure ()

-- | Fan out events to two handlers.
combineHandlers :: EventHandler -> EventHandler -> EventHandler
combineHandlers h1 h2 evt = h1 evt >> h2 evt

-- | Emit an event to a handler if present. No-op when 'Nothing'.
emitEvent :: Maybe EventHandler -> APIEvent -> IO ()
emitEvent Nothing  _   = pure ()
emitEvent (Just h) evt = h evt
