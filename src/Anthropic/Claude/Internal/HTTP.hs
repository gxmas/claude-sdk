{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Anthropic.Claude.Internal.HTTP
Description : HTTP client infrastructure
Copyright   : (c) 2026 Anthropic
License     : MIT
Stability   : internal

Internal HTTP client infrastructure for making authenticated requests
to the Claude API. Handles request construction, header setting,
and response parsing with rate limit extraction.
-}
module Anthropic.Claude.Internal.HTTP
  ( -- * Client Environment
    mkClientEnv
  , defaultClientEnv

    -- * HTTP Operations
  , buildRequest
  , executeRequest
  , parseResponse

    -- * Rate Limit Extraction
  , extractRateLimitInfo
  ) where

import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, Request, RequestBody(..), Response, httpLbs, newManager, parseRequest)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status(..))
import qualified Data.Aeson as Aeson

-- API constants
apiBaseUrl :: String
apiBaseUrl = "https://api.anthropic.com"

apiVersion :: BS.ByteString
apiVersion = "2023-06-01"

userAgent :: BS.ByteString
userAgent = "claude-sdk-haskell/0.1.0.0"

-- | Create a new client environment with TLS manager
--
-- Sets up HTTP connection pooling via the TLS manager.
--
-- Example:
-- @
-- env <- mkClientEnv (ApiKey "sk-ant-...")
-- @
mkClientEnv :: ApiKey -> IO ClientEnv
mkClientEnv apiKey = do
  manager <- newManager tlsManagerSettings
  pure ClientEnv
    { clientApiKey = apiKey
    , clientRetryPolicy = defaultRetryPolicy
    , clientBaseUrl = apiBaseUrl
    , clientManager = manager
    , clientEventHandler = Nothing
    , clientLogSettings = Nothing
    }

-- | Default client environment (for testing)
defaultClientEnv :: ApiKey -> Manager -> ClientEnv
defaultClientEnv apiKey manager = ClientEnv
  { clientApiKey = apiKey
  , clientRetryPolicy = defaultRetryPolicy
  , clientBaseUrl = apiBaseUrl
  , clientManager = manager
  , clientEventHandler = Nothing
  , clientLogSettings = Nothing
  }

-- | Build an HTTP request with proper headers
--
-- Sets required headers:
-- - @x-api-key@: Authentication
-- - @anthropic-version@: API version
-- - @content-type@: application/json
-- - @user-agent@: SDK identifier
buildRequest
  :: ClientEnv
  -> Method
  -> String        -- ^ Path (e.g., "/v1/messages")
  -> RequestBody
  -> IO Request
buildRequest ClientEnv{..} httpMethod path body = do
  baseReq <- parseRequest $ clientBaseUrl <> path
  pure baseReq
    { HTTP.method = httpMethod
    , HTTP.requestHeaders =
        [ ("x-api-key", TE.encodeUtf8 $ unApiKey clientApiKey)
        , ("anthropic-version", apiVersion)
        , ("content-type", "application/json")
        , ("user-agent", userAgent)
        ]
    , HTTP.requestBody = body
    }

-- | Execute an HTTP request and return the response
executeRequest :: Manager -> Request -> IO (Response LBS.ByteString)
executeRequest manager req = httpLbs req manager

-- | Parse HTTP response into Either APIError a
--
-- Maps status codes to APIError variants and extracts rate limit headers.
parseResponse
  :: Aeson.FromJSON a
  => Response LBS.ByteString
  -> Maybe RequestId
  -> Either APIError a
parseResponse response _reqId =
  let status = HTTP.responseStatus response
      bodyLBS = HTTP.responseBody response
  in if statusCode status == 200
       then case Aeson.eitherDecode bodyLBS of
         Left err -> Left $ APIError
           { errorKind = InvalidRequestError
           , errorDetails = ErrorDetails "parse_error" (T.pack err)
           , errorStatusCode = 200
           }
         Right parsed -> Right parsed
       else case Aeson.eitherDecode bodyLBS of
         Left _ -> Left $ APIError
           { errorKind = errorKindFromStatus status
           , errorDetails = ErrorDetails "unknown_error" "Failed to parse error response"
           , errorStatusCode = statusCode status
           }
         Right details -> Left $ APIError
           { errorKind = errorKindFromStatus status
           , errorDetails = details
           , errorStatusCode = statusCode status
           }

-- | Extract rate limit information from response headers
--
-- Parses headers like:
-- - @anthropic-ratelimit-requests-limit@
-- - @anthropic-ratelimit-requests-remaining@
-- - @anthropic-ratelimit-requests-reset@
-- - @anthropic-ratelimit-tokens-limit@
-- - @anthropic-ratelimit-tokens-remaining@
-- - @anthropic-ratelimit-tokens-reset@
extractRateLimitInfo :: [Header] -> Maybe RateLimitInfo
extractRateLimitInfo headers =
  let rateLimitRequests = lookupInt "anthropic-ratelimit-requests-limit" headers
      rateLimitTokens = lookupInt "anthropic-ratelimit-tokens-limit" headers
      rateLimitRemaining = lookupInt "anthropic-ratelimit-requests-remaining" headers
      rateLimitTokensRemaining = lookupInt "anthropic-ratelimit-tokens-remaining" headers
      rateLimitResetRequests = lookupInt "anthropic-ratelimit-requests-reset" headers
      rateLimitResetTokens = lookupInt "anthropic-ratelimit-tokens-reset" headers
  in if all (== Nothing) [rateLimitRequests, rateLimitTokens, rateLimitRemaining, rateLimitTokensRemaining, rateLimitResetRequests, rateLimitResetTokens]
       then Nothing
       else Just RateLimitInfo
         { rateLimitRequests = rateLimitRequests
         , rateLimitTokens = rateLimitTokens
         , rateLimitRemaining = rateLimitRemaining
         , rateLimitTokensRemaining = rateLimitTokensRemaining
         , rateLimitResetRequests = rateLimitResetRequests
         , rateLimitResetTokens = rateLimitResetTokens
         }

-- | Lookup a header value and parse as Int
lookupInt :: HeaderName -> [Header] -> Maybe Int
lookupInt name headers = do
  value <- lookup name headers
  case BS8.readInt value of
    Just (i, _) -> Just i
    Nothing -> Nothing
