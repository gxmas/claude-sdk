{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Logging
Description : Development/debugging logging for Claude SDK
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Logging layer for development and debugging. Provides @curl -v@ style
output of HTTP requests and responses, retry decisions, and errors.

Independent from the observability event system. Observability emits
structured 'APIEvent' values for programmatic consumption (metrics,
tracing). Logging emits human-readable text for developer eyeballs.

Zero-cost when unused: if no logger is set ('Nothing'), no log
messages are formatted or emitted.
-}
module Anthropic.Claude.Types.Logging
  ( -- * Log Levels
    LogLevel(..)

    -- * Logger
  , Logger
  , LogSettings(..)
  , defaultLogSettings

    -- * Pre-built Loggers
  , stderrLogger
  , debugLogger

    -- * Internal Helpers
  , logMsg
  , logHttpRequest
  , logHttpResponse
  , logRetryAttempt
  , logApiError

    -- * Formatting Utilities
  , truncateBody
  , maskApiKey
  , showDurationMs
  , showLevel
  ) where

import Anthropic.Claude.Types.Client (RateLimitInfo(..))
import Anthropic.Claude.Types.Core (RequestId(..))
import Anthropic.Claude.Types.Error (APIError(..), ErrorDetails(..), isRetryable)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (stderr)

-- | Log levels in increasing severity.
--
-- Filtering uses 'Ord': a logger created with 'stderrLogger' 'LevelInfo'
-- will emit 'LevelInfo', 'LevelWarn', and 'LevelError' messages but
-- suppress 'LevelDebug'.
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A logger receives a level and a message.
--
-- The logger implementation decides whether to emit based on the level.
type Logger = LogLevel -> Text -> IO ()

-- | Logging configuration stored in 'ClientEnv'.
data LogSettings = LogSettings
  { logLogger    :: Logger
  , logBodyLimit :: Int      -- ^ Max bytes of request/response body in debug output
  }

-- | Default log settings: 4096-byte body limit.
defaultLogSettings :: Logger -> LogSettings
defaultLogSettings l = LogSettings l 4096

-- | Display name for a log level, padded to 5 chars.
showLevel :: LogLevel -> Text
showLevel LevelDebug = "DEBUG"
showLevel LevelInfo  = "INFO "
showLevel LevelWarn  = "WARN "
showLevel LevelError = "ERROR"

-- ---------------------------------------------------------------------------
-- Pre-built loggers
-- ---------------------------------------------------------------------------

-- | Logger that writes to stderr, filtering at the given minimum level.
--
-- Output format:
-- @[HH:MM:SS] [LEVEL] message@
--
-- Example:
-- @
-- env <- withLogger (stderrLogger LevelDebug) <$> mkClientEnv key
-- @
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel lvl msg
  | lvl >= minLevel = do
      now <- getCurrentTime
      TIO.hPutStrLn stderr $
        "[" <> formatTimestamp now <> "] [" <> showLevel lvl <> "] " <> msg
  | otherwise = pure ()

-- | Convenience logger: logs everything to stderr.
debugLogger :: Logger
debugLogger = stderrLogger LevelDebug

-- ---------------------------------------------------------------------------
-- Zero-cost dispatch
-- ---------------------------------------------------------------------------

-- | Emit a log message if a logger is configured. No-op when 'Nothing'.
logMsg :: Maybe LogSettings -> LogLevel -> Text -> IO ()
logMsg Nothing  _   _   = pure ()
logMsg (Just s) lvl msg = logLogger s lvl msg

-- ---------------------------------------------------------------------------
-- Structured log helpers
-- ---------------------------------------------------------------------------

-- | Log an HTTP request at Debug level.
logHttpRequest :: Maybe LogSettings -> Text -> Text -> LBS.ByteString -> IO ()
logHttpRequest Nothing _ _ _ = pure ()
logHttpRequest (Just settings) method path body =
  logLogger settings LevelDebug $
    "→ " <> method <> " " <> path <> "\n"
    <> "  Body: " <> truncateBody (logBodyLimit settings) body

-- | Log an HTTP response at Debug and Info levels.
--
-- Also emits a Warn if rate limits are below 10% remaining.
logHttpResponse
  :: Maybe LogSettings
  -> Text              -- ^ HTTP method
  -> Text              -- ^ Path
  -> Int               -- ^ Status code
  -> NominalDiffTime   -- ^ Duration
  -> Maybe RequestId
  -> Maybe RateLimitInfo
  -> Maybe LBS.ByteString  -- ^ Response body ('Nothing' for streaming)
  -> IO ()
logHttpResponse Nothing _ _ _ _ _ _ _ = pure ()
logHttpResponse (Just settings) method path status duration reqId rateInfo mBody = do
  let logger = logLogger settings
      reqIdTxt = maybe "" (\(RequestId r) -> " " <> r) reqId
      durationTxt = showDurationMs duration

  -- Debug: full response dump
  case mBody of
    Just body ->
      logger LevelDebug $
        "← " <> T.pack (show status) <> " (" <> durationTxt <> ")"
        <> reqIdTxt <> "\n"
        <> "  Body: " <> truncateBody (logBodyLimit settings) body
    Nothing ->
      logger LevelDebug $
        "← " <> T.pack (show status) <> " (" <> durationTxt <> ")"
        <> reqIdTxt <> " [streaming]"

  -- Info: one-line summary
  logger LevelInfo $
    method <> " " <> path <> " → " <> T.pack (show status)
    <> " (" <> durationTxt <> ")"

  -- Warn: rate limit pressure (< 10% remaining)
  case rateInfo of
    Just rl ->
      case (rateLimitRemaining rl, rateLimitRequests rl) of
        (Just remaining, Just total) | total > 0 && remaining * 10 < total ->
          logger LevelWarn $
            "Rate limit low: " <> T.pack (show remaining) <> "/"
            <> T.pack (show total) <> " requests remaining"
        _ -> pure ()
    Nothing -> pure ()

-- | Log a retry attempt at Warn level.
logRetryAttempt
  :: Maybe LogSettings
  -> APIError          -- ^ The error that triggered the retry
  -> Int               -- ^ 1-based attempt number
  -> Int               -- ^ Max attempts
  -> NominalDiffTime   -- ^ Backoff delay
  -> IO ()
logRetryAttempt Nothing _ _ _ _ = pure ()
logRetryAttempt (Just settings) err attempt maxAttempts backoff =
  logLogger settings LevelWarn $
    "Retry " <> T.pack (show attempt) <> "/" <> T.pack (show maxAttempts)
    <> ": " <> T.pack (show (errorStatusCode err))
    <> " " <> errorType (errorDetails err)
    <> " — backing off " <> showDurationMs backoff

-- | Log a non-retryable API error at Error level.
--
-- Retryable errors are not logged here — they are handled by
-- 'logRetryAttempt' in the retry layer.
logApiError :: Maybe LogSettings -> APIError -> IO ()
logApiError Nothing _ = pure ()
logApiError (Just settings) err
  | isRetryable err = pure ()
  | otherwise =
      logLogger settings LevelError $
        "API error " <> T.pack (show (errorStatusCode err)) <> ": "
        <> errorType (errorDetails err) <> ": "
        <> errorMessage (errorDetails err)

-- ---------------------------------------------------------------------------
-- Formatting utilities
-- ---------------------------------------------------------------------------

-- | Truncate a response body for log output.
--
-- Decodes as UTF-8. If the body exceeds the limit, truncates and appends
-- a note showing how many bytes were omitted. Binary bodies that fail
-- UTF-8 decoding show as @\<binary N bytes\>@.
truncateBody :: Int -> LBS.ByteString -> Text
truncateBody _ body | LBS.null body = "<empty>"
truncateBody limit body
  | LBS.length body <= fromIntegral limit =
      decodeBody (LBS.toStrict body)
  | otherwise =
      decodeBody (LBS.toStrict (LBS.take (fromIntegral limit) body))
      <> "... (" <> T.pack (show (LBS.length body - fromIntegral limit))
      <> " bytes truncated)"
  where
    decodeBody bs = case TE.decodeUtf8' bs of
      Right txt -> txt
      Left _    -> "<binary " <> T.pack (show (BS.length bs)) <> " bytes>"

-- | Mask an API key for safe logging.
--
-- @"sk-ant-api03-abc...xyz"@ becomes @"sk-ant-***"@.
maskApiKey :: Text -> Text
maskApiKey key
  | "sk-ant-" `T.isPrefixOf` key = "sk-ant-***"
  | T.length key > 6 = T.take 6 key <> "***"
  | otherwise = "***"

-- | Format a duration in milliseconds.
showDurationMs :: NominalDiffTime -> Text
showDurationMs dt =
  let ms = round (dt * 1000) :: Int
  in T.pack (show ms) <> "ms"

-- | Format a UTCTime as @HH:MM:SS@.
formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%T"
