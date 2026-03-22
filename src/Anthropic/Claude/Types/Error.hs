{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Error
Description : Error types for Claude SDK
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Error types for the Claude SDK following ADR 0002:
- Network/infrastructure errors thrown as exceptions
- API errors (4xx, 5xx) returned in Either
-}
module Anthropic.Claude.Types.Error
  ( -- * API Errors
    APIError(..)
  , APIErrorKind(..)
  , ErrorDetails(..)

    -- * Network Errors (Exceptions)
  , NetworkError(..)

    -- * Helper Functions
  , isRetryable
  , errorKindFromStatus
  ) where

import Anthropic.Claude.Internal.JSON
import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (Status(..))

-- | Error details from API error responses
data ErrorDetails = ErrorDetails
  { errorType :: Text        -- ^ Error type (e.g., "invalid_request_error")
  , errorMessage :: Text     -- ^ Human-readable error message
  } deriving (Eq, Show, Generic)

instance FromJSON ErrorDetails where
  parseJSON = withObject "ErrorDetails" $ \obj -> do
    errObj <- obj .: "error"
    ErrorDetails
      <$> errObj .: "type"
      <*> errObj .: "message"

instance ToJSON ErrorDetails where
  toJSON (ErrorDetails typ msg) = object
    [ "error" .= object
        [ "type" .= typ
        , "message" .= msg
        ]
    ]

-- | Classification of API error types
data APIErrorKind
  = InvalidRequestError     -- ^ 400 - Malformed request
  | AuthenticationError     -- ^ 401 - Invalid API key
  | PermissionError         -- ^ 403 - Insufficient permissions
  | NotFoundError           -- ^ 404 - Resource not found
  | RateLimitError          -- ^ 429 - Rate limit exceeded
  | ServerError             -- ^ 500 - Internal server error
  | OverloadedError         -- ^ 529 - Service temporarily overloaded
  deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON APIErrorKind where
  parseJSON = withText "APIErrorKind" $ \case
    "invalid_request_error" -> pure InvalidRequestError
    "authentication_error" -> pure AuthenticationError
    "permission_error" -> pure PermissionError
    "not_found_error" -> pure NotFoundError
    "rate_limit_error" -> pure RateLimitError
    "api_error" -> pure ServerError
    "overloaded_error" -> pure OverloadedError
    other -> fail $ "Unknown error kind: " <> T.unpack other

instance ToJSON APIErrorKind where
  toJSON InvalidRequestError = "invalid_request_error"
  toJSON AuthenticationError = "authentication_error"
  toJSON PermissionError = "permission_error"
  toJSON NotFoundError = "not_found_error"
  toJSON RateLimitError = "rate_limit_error"
  toJSON ServerError = "api_error"
  toJSON OverloadedError = "overloaded_error"

-- | API error returned by Claude API
--
-- Per ADR 0002, these are returned in Either, not thrown as exceptions.
-- This allows partial recovery from stream errors.
data APIError = APIError
  { errorKind :: APIErrorKind
  , errorDetails :: ErrorDetails
  , errorStatusCode :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON APIError where
  parseJSON = withObject "APIError" $ \obj -> do
    details <- parseJSON (Object obj)
    let kind = case errorType details of
          "invalid_request_error" -> InvalidRequestError
          "authentication_error" -> AuthenticationError
          "permission_error" -> PermissionError
          "not_found_error" -> NotFoundError
          "rate_limit_error" -> RateLimitError
          "api_error" -> ServerError
          "overloaded_error" -> OverloadedError
          _ -> ServerError  -- Default to server error for unknown types
    status <- obj .:? "status" .!= 500
    pure $ APIError kind details status

instance ToJSON APIError where
  toJSON (APIError kind details status) = object
    [ "error" .= object
        [ "type" .= toJSON kind
        , "message" .= errorMessage details
        ]
    , "status" .= status
    ]

-- | Network errors (thrown as exceptions per ADR 0002)
--
-- These represent infrastructure failures, not API errors:
-- - Connection failures
-- - DNS resolution errors
-- - TLS handshake failures
-- - Timeout errors
data NetworkError
  = ConnectionError Text      -- ^ Failed to establish connection
  | TimeoutError Text         -- ^ Request timed out
  | TLSError Text             -- ^ TLS/SSL error
  | DNSError Text             -- ^ DNS resolution failed
  | UnknownNetworkError Text  -- ^ Other network error
  deriving (Eq, Show, Generic)

instance Exception NetworkError

-- | Check if an API error is retryable
--
-- Per ADR 0003, only certain errors should trigger retries:
-- - 429 Rate Limit - Always retry with backoff
-- - 500 Server Error - Retry (transient)
-- - 529 Overloaded - Retry with longer backoff
--
-- Non-retryable (permanent failures):
-- - 400 Invalid Request
-- - 401 Authentication Error
-- - 403 Permission Error
-- - 404 Not Found
isRetryable :: APIError -> Bool
isRetryable (APIError kind _ _) = case kind of
  RateLimitError -> True
  ServerError -> True
  OverloadedError -> True
  _ -> False

-- | Determine error kind from HTTP status code
errorKindFromStatus :: Status -> APIErrorKind
errorKindFromStatus (Status code _) = case code of
  400 -> InvalidRequestError
  401 -> AuthenticationError
  403 -> PermissionError
  404 -> NotFoundError
  429 -> RateLimitError
  500 -> ServerError
  529 -> OverloadedError
  _ | code >= 500 -> ServerError
    | otherwise -> InvalidRequestError
