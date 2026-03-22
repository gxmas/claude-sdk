{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Types.ObservabilitySpec (spec) where

import Anthropic.Claude.Types.Client (RateLimitInfo(..))
import Anthropic.Claude.Types.Core (RequestId(..))
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Observability
import Data.IORef
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Test.Hspec

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2026 3 22) 0

sampleError :: APIError
sampleError = APIError RateLimitError (ErrorDetails "rate_limit_error" "Rate limit exceeded") 429

spec :: Spec
spec = describe "Types.Observability" $ do

  describe "emitEvent" $ do
    it "is a no-op when handler is Nothing" $ do
      ref <- newIORef (0 :: Int)
      let evt = HttpRequest HttpRequestEvent
            { reqMethod = "POST", reqPath = "/v1/messages", reqTimestamp = sampleTime }
      emitEvent Nothing evt
      count <- readIORef ref
      count `shouldBe` 0

    it "invokes handler when Just" $ do
      ref <- newIORef ([] :: [APIEvent])
      let handler e = modifyIORef' ref (e :)
          evt = HttpRequest HttpRequestEvent
            { reqMethod = "GET", reqPath = "/v1/messages/batches", reqTimestamp = sampleTime }
      emitEvent (Just handler) evt
      events <- readIORef ref
      events `shouldBe` [evt]

    it "invokes handler with HttpResponse event" $ do
      ref <- newIORef ([] :: [APIEvent])
      let handler e = modifyIORef' ref (e :)
          evt = HttpResponse HttpResponseEvent
            { respStatusCode = 200
            , respDuration = 0.5
            , respRequestId = Just (RequestId "req_123")
            , respRateLimitInfo = Nothing
            }
      emitEvent (Just handler) evt
      events <- readIORef ref
      length events `shouldBe` 1

    it "invokes handler with Retry event" $ do
      ref <- newIORef ([] :: [APIEvent])
      let handler e = modifyIORef' ref (e :)
          evt = Retry RetryEvent
            { retryEvtError = sampleError
            , retryEvtAttempt = 1
            , retryEvtMaxAttempts = 3
            , retryEvtBackoff = 1.0
            }
      emitEvent (Just handler) evt
      events <- readIORef ref
      events `shouldBe` [evt]

  describe "combineHandlers" $ do
    it "calls both handlers" $ do
      ref1 <- newIORef (0 :: Int)
      ref2 <- newIORef (0 :: Int)
      let h1 _ = modifyIORef' ref1 (+ 1)
          h2 _ = modifyIORef' ref2 (+ 1)
          combined = combineHandlers h1 h2
          evt = HttpRequest HttpRequestEvent
            { reqMethod = "POST", reqPath = "/v1/messages", reqTimestamp = sampleTime }
      combined evt
      c1 <- readIORef ref1
      c2 <- readIORef ref2
      c1 `shouldBe` 1
      c2 `shouldBe` 1

    it "calls handlers in order" $ do
      ref <- newIORef ([] :: [String])
      let h1 _ = modifyIORef' ref (++ ["first"])
          h2 _ = modifyIORef' ref (++ ["second"])
          combined = combineHandlers h1 h2
          evt = HttpRequest HttpRequestEvent
            { reqMethod = "POST", reqPath = "/test", reqTimestamp = sampleTime }
      combined evt
      order <- readIORef ref
      order `shouldBe` ["first", "second"]

  describe "noOpHandler" $ do
    it "does not crash" $ do
      let evt = HttpRequest HttpRequestEvent
            { reqMethod = "POST", reqPath = "/v1/messages", reqTimestamp = sampleTime }
      noOpHandler evt
      -- If we get here without exception, the test passes

  describe "Event constructors" $ do
    it "HttpRequestEvent carries expected fields" $ do
      let evt = HttpRequestEvent
            { reqMethod = "POST"
            , reqPath = "/v1/messages"
            , reqTimestamp = sampleTime
            }
      reqMethod evt `shouldBe` "POST"
      reqPath evt `shouldBe` "/v1/messages"
      reqTimestamp evt `shouldBe` sampleTime

    it "HttpResponseEvent carries expected fields" $ do
      let rateInfo = Just $ RateLimitInfo (Just 50) (Just 1000) (Just 49) (Just 999) Nothing Nothing
          evt = HttpResponseEvent
            { respStatusCode = 200
            , respDuration = 0.123
            , respRequestId = Just (RequestId "req_abc")
            , respRateLimitInfo = rateInfo
            }
      respStatusCode evt `shouldBe` 200
      respDuration evt `shouldBe` 0.123
      respRequestId evt `shouldBe` Just (RequestId "req_abc")
      respRateLimitInfo evt `shouldBe` rateInfo

    it "RetryEvent carries expected fields" $ do
      let evt = RetryEvent
            { retryEvtError = sampleError
            , retryEvtAttempt = 2
            , retryEvtMaxAttempts = 5
            , retryEvtBackoff = 4.0
            }
      retryEvtAttempt evt `shouldBe` 2
      retryEvtMaxAttempts evt `shouldBe` 5
      retryEvtBackoff evt `shouldBe` 4.0
      retryEvtError evt `shouldBe` sampleError
