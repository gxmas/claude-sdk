{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Types.LoggingSpec (spec) where

import Anthropic.Claude.Types.Client (RateLimitInfo(..))
import Anthropic.Claude.Types.Core (RequestId(..))
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Logging
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- | Capture logger: records all messages at or above minLevel.
captureLogger :: LogLevel -> IO (Logger, IO [(LogLevel, Text)])
captureLogger minLevel = do
  ref <- newIORef ([] :: [(LogLevel, Text)])
  let logger lvl msg
        | lvl >= minLevel = modifyIORef' ref ((lvl, msg) :)
        | otherwise = pure ()
  pure (logger, reverse <$> readIORef ref)

sampleError :: APIError
sampleError = APIError RateLimitError (ErrorDetails "rate_limit_error" "Rate limit exceeded") 429

nonRetryableError :: APIError
nonRetryableError = APIError InvalidRequestError (ErrorDetails "invalid_request_error" "max_tokens must be positive") 400

spec :: Spec
spec = describe "Types.Logging" $ do

  describe "LogLevel ordering" $ do
    it "Debug < Info < Warn < Error" $ do
      LevelDebug < LevelInfo `shouldBe` True
      LevelInfo < LevelWarn `shouldBe` True
      LevelWarn < LevelError `shouldBe` True

  describe "logMsg" $ do
    it "is a no-op when Nothing" $ do
      ref <- newIORef (0 :: Int)
      let settings = Nothing
      logMsg settings LevelInfo "test"
      count <- readIORef ref
      count `shouldBe` 0

    it "invokes logger when Just" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logMsg settings LevelInfo "hello"
      msgs <- getMessages
      msgs `shouldBe` [(LevelInfo, "hello")]

  describe "stderrLogger filtering" $ do
    it "filters below minimum level" $ do
      -- stderrLogger writes to stderr, so test via captureLogger which mimics the pattern
      (logger, getMessages) <- captureLogger LevelWarn
      let settings = Just (LogSettings logger 4096)
      logMsg settings LevelDebug "debug msg"
      logMsg settings LevelInfo "info msg"
      logMsg settings LevelWarn "warn msg"
      logMsg settings LevelError "error msg"
      msgs <- getMessages
      map fst msgs `shouldBe` [LevelWarn, LevelError]

  describe "truncateBody" $ do
    it "handles empty body" $
      truncateBody 4096 "" `shouldBe` "<empty>"

    it "passes through small body" $
      truncateBody 4096 "{\"key\":\"value\"}" `shouldBe` "{\"key\":\"value\"}"

    it "truncates body exceeding limit" $ do
      let body = LBS8.pack (replicate 100 'x')
      let result = truncateBody 50 body
      T.isInfixOf "truncated" result `shouldBe` True
      T.isInfixOf "50 bytes truncated" result `shouldBe` True

    it "shows binary for non-UTF8" $ do
      let body = LBS.pack [0xFF, 0xFE, 0x00, 0x01]
      let result = truncateBody 4096 body
      T.isInfixOf "binary" result `shouldBe` True

  describe "maskApiKey" $ do
    it "masks sk-ant- prefixed keys" $
      maskApiKey "sk-ant-api03-abcdef123456" `shouldBe` "sk-ant-***"

    it "masks other long keys" $
      maskApiKey "some-other-key-12345" `shouldBe` "some-o***"

    it "masks short keys entirely" $
      maskApiKey "short" `shouldBe` "***"

  describe "showDurationMs" $ do
    it "formats sub-second durations" $
      showDurationMs 0.342 `shouldBe` "342ms"

    it "formats multi-second durations" $
      showDurationMs 1.5 `shouldBe` "1500ms"

    it "formats zero duration" $
      showDurationMs 0 `shouldBe` "0ms"

  describe "showLevel" $ do
    it "formats all levels" $ do
      showLevel LevelDebug `shouldBe` "DEBUG"
      showLevel LevelInfo `shouldBe` "INFO "
      showLevel LevelWarn `shouldBe` "WARN "
      showLevel LevelError `shouldBe` "ERROR"

  describe "logHttpRequest" $ do
    it "is a no-op when Nothing" $ do
      logHttpRequest Nothing "POST" "/v1/messages" "{}"
      -- No crash = pass

    it "emits Debug-level message with body" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logHttpRequest settings "POST" "/v1/messages" "{\"model\":\"test\"}"
      msgs <- getMessages
      length msgs `shouldBe` 1
      fst (head msgs) `shouldBe` LevelDebug
      T.isInfixOf "POST" (snd (head msgs)) `shouldBe` True
      T.isInfixOf "model" (snd (head msgs)) `shouldBe` True

  describe "logHttpResponse" $ do
    it "emits Debug + Info messages" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
          body = Just "{\"id\":\"msg_123\"}"
      logHttpResponse settings "POST" "/v1/messages" 200 0.5
        (Just (RequestId "req_abc")) Nothing body
      msgs <- getMessages
      map fst msgs `shouldBe` [LevelDebug, LevelInfo]
      -- Debug has body
      T.isInfixOf "msg_123" (snd (head msgs)) `shouldBe` True
      -- Info has summary
      T.isInfixOf "200" (snd (msgs !! 1)) `shouldBe` True
      T.isInfixOf "500ms" (snd (msgs !! 1)) `shouldBe` True

    it "shows [streaming] for Nothing body" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logHttpResponse settings "POST" "/v1/messages" 200 0.1
        Nothing Nothing Nothing
      msgs <- getMessages
      T.isInfixOf "[streaming]" (snd (head msgs)) `shouldBe` True

    it "emits rate limit warning when low" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
          rateInfo = Just $ RateLimitInfo (Just 50) Nothing (Just 3) Nothing Nothing Nothing
      logHttpResponse settings "GET" "/v1/test" 200 0.1
        Nothing rateInfo (Just "{}")
      msgs <- getMessages
      let warnMsgs = filter ((== LevelWarn) . fst) msgs
      length warnMsgs `shouldBe` 1
      T.isInfixOf "3/50" (snd (head warnMsgs)) `shouldBe` True

    it "no rate limit warning when sufficient" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
          rateInfo = Just $ RateLimitInfo (Just 50) Nothing (Just 45) Nothing Nothing Nothing
      logHttpResponse settings "GET" "/v1/test" 200 0.1
        Nothing rateInfo (Just "{}")
      msgs <- getMessages
      let warnMsgs = filter ((== LevelWarn) . fst) msgs
      length warnMsgs `shouldBe` 0

  describe "logRetryAttempt" $ do
    it "emits Warn-level message" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logRetryAttempt settings sampleError 1 3 1.0
      msgs <- getMessages
      length msgs `shouldBe` 1
      fst (head msgs) `shouldBe` LevelWarn
      T.isInfixOf "Retry 1/3" (snd (head msgs)) `shouldBe` True
      T.isInfixOf "429" (snd (head msgs)) `shouldBe` True
      T.isInfixOf "1000ms" (snd (head msgs)) `shouldBe` True

  describe "logApiError" $ do
    it "logs non-retryable errors at Error level" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logApiError settings nonRetryableError
      msgs <- getMessages
      length msgs `shouldBe` 1
      fst (head msgs) `shouldBe` LevelError
      T.isInfixOf "400" (snd (head msgs)) `shouldBe` True
      T.isInfixOf "max_tokens" (snd (head msgs)) `shouldBe` True

    it "skips retryable errors" $ do
      (logger, getMessages) <- captureLogger LevelDebug
      let settings = Just (LogSettings logger 4096)
      logApiError settings sampleError
      msgs <- getMessages
      length msgs `shouldBe` 0

    it "is a no-op when Nothing" $ do
      logApiError Nothing nonRetryableError
      -- No crash = pass

  describe "defaultLogSettings" $ do
    it "uses 4096 byte body limit" $ do
      (logger, _) <- captureLogger LevelDebug
      let settings = defaultLogSettings logger
      logBodyLimit settings `shouldBe` 4096
