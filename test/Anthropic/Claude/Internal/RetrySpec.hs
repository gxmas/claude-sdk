{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Internal.RetrySpec (spec) where

import Anthropic.Claude.Internal.Retry
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Observability
import Data.IORef
import Test.Hspec
import UnliftIO (liftIO)

-- | Build a test ClientEnv with the given RetryPolicy and optional event handler.
testEnv :: RetryPolicy -> Maybe EventHandler -> ClientEnv
testEnv policy handler =
  ClientEnv
    { clientApiKey = ApiKey "test"
    , clientRetryPolicy = policy
    , clientBaseUrl = "https://api.anthropic.com"
    , clientManager = undefined -- Not used in retry tests
    , clientEventHandler = handler
    , clientLogSettings = Nothing
    }

-- Tests

spec :: Spec
spec = describe "Internal.Retry" $ do
  describe "shouldRetryError" $ do
    it "returns True for RateLimitError" $ do
      let err = APIError RateLimitError (ErrorDetails "rate_limit_error" "msg") 429
      shouldRetryError err `shouldBe` True

    it "returns True for ServerError" $ do
      let err = APIError ServerError (ErrorDetails "api_error" "msg") 500
      shouldRetryError err `shouldBe` True

    it "returns True for OverloadedError" $ do
      let err = APIError OverloadedError (ErrorDetails "overloaded_error" "msg") 529
      shouldRetryError err `shouldBe` True

    it "returns False for InvalidRequestError" $ do
      let err = APIError InvalidRequestError (ErrorDetails "invalid_request_error" "msg") 400
      shouldRetryError err `shouldBe` False

    it "returns False for AuthenticationError" $ do
      let err = APIError AuthenticationError (ErrorDetails "authentication_error" "msg") 401
      shouldRetryError err `shouldBe` False

    it "returns False for PermissionError" $ do
      let err = APIError PermissionError (ErrorDetails "permission_error" "msg") 403
      shouldRetryError err `shouldBe` False

    it "returns False for NotFoundError" $ do
      let err = APIError NotFoundError (ErrorDetails "not_found_error" "msg") 404
      shouldRetryError err `shouldBe` False

  describe "calculateBackoff" $ do
    it "exponential backoff increases correctly" $ do
      let policy = RetryPolicy 5 (ExponentialBackoff 1.0 60.0)
      calculateBackoff policy 0 `shouldBe` 1.0
      calculateBackoff policy 1 `shouldBe` 2.0
      calculateBackoff policy 2 `shouldBe` 4.0
      calculateBackoff policy 3 `shouldBe` 8.0
      calculateBackoff policy 4 `shouldBe` 16.0

    it "exponential backoff respects max delay cap" $ do
      let policy = RetryPolicy 10 (ExponentialBackoff 1.0 10.0)
      calculateBackoff policy 5 `shouldBe` 10.0 -- 32 capped to 10
      calculateBackoff policy 10 `shouldBe` 10.0 -- 1024 capped to 10
    it "constant backoff returns fixed delay" $ do
      let policy = RetryPolicy 5 (ConstantBackoff 2.5)
      calculateBackoff policy 0 `shouldBe` 2.5
      calculateBackoff policy 1 `shouldBe` 2.5
      calculateBackoff policy 10 `shouldBe` 2.5

    it "no backoff returns zero" $ do
      let policy = RetryPolicy 5 NoBackoff
      calculateBackoff policy 0 `shouldBe` 0
      calculateBackoff policy 5 `shouldBe` 0

  describe "withRetry" $ do
    it "succeeds immediately on Right" $ do
      let env = testEnv (RetryPolicy 3 NoBackoff) Nothing
      result <- withRetry env (pure $ Right (42 :: Int))
      result `shouldBe` Right 42

    it "fails immediately on non-retryable error" $ do
      counter <- newIORef (0 :: Int)
      let env = testEnv (RetryPolicy 3 NoBackoff) Nothing
          err = APIError InvalidRequestError (ErrorDetails "invalid_request_error" "msg") 400
          action :: IO (Either APIError Int)
          action = do
            modifyIORef' counter (+ 1)
            pure $ Left err

      result <- withRetry env action
      count <- readIORef counter

      result `shouldBe` Left err
      count `shouldBe` 1 -- Should only attempt once
    it "retries on retryable errors" $ do
      counter <- newIORef (0 :: Int)
      let env = testEnv (RetryPolicy 3 NoBackoff) Nothing
          err = APIError RateLimitError (ErrorDetails "rate_limit_error" "msg") 429
          action :: IO (Either APIError Int)
          action = do
            modifyIORef' counter (+ 1)
            count <- readIORef counter
            if count < 3
              then pure $ Left err
              else pure $ Right (42 :: Int)

      result <- withRetry env action
      count <- readIORef counter

      result `shouldBe` Right 42
      count `shouldBe` 3 -- Initial + 2 retries
    it "respects max attempts limit" $ do
      counter <- newIORef (0 :: Int)
      let env = testEnv (RetryPolicy 2 NoBackoff) Nothing
          err = APIError ServerError (ErrorDetails "api_error" "msg") 500
          action :: IO (Either APIError Int)
          action = do
            modifyIORef' counter (+ 1)
            pure $ Left err

      result <- withRetry env action
      count <- readIORef counter

      result `shouldBe` Left err
      count `shouldBe` 3 -- Initial attempt + 2 retries
    it "stops retrying once successful" $ do
      counter <- newIORef (0 :: Int)
      let env = testEnv (RetryPolicy 5 NoBackoff) Nothing
          err = APIError RateLimitError (ErrorDetails "rate_limit_error" "msg") 429
          action :: IO (Either APIError Int)
          action = do
            modifyIORef' counter (+ 1)
            count <- readIORef counter
            if count < 2
              then pure $ Left err
              else pure $ Right (42 :: Int)

      result <- withRetry env action
      count <- readIORef counter

      result `shouldBe` Right 42
      count `shouldBe` 2 -- Should stop after success, not continue to max attempts
    it "emits RetryEvent via event handler" $ do
      eventsRef <- newIORef ([] :: [APIEvent])
      let handler evt = modifyIORef' eventsRef (evt :)
          env = testEnv (RetryPolicy 3 NoBackoff) (Just handler)
          err = APIError RateLimitError (ErrorDetails "rate_limit_error" "msg") 429
      counter <- newIORef (0 :: Int)
      let action :: IO (Either APIError Int)
          action = do
            modifyIORef' counter (+ 1)
            count <- readIORef counter
            if count < 3
              then pure $ Left err
              else pure $ Right 42

      result <- withRetry env action
      result `shouldBe` Right 42

      events <- readIORef eventsRef
      -- Should have 2 retry events (attempt 1 and 2)
      length events `shouldBe` 2
      -- Events are prepended, so reverse to get chronological order
      let eventsInOrder = reverse events
      case eventsInOrder of
        [evt1, evt2] -> do
          case evt1 of
            Retry re -> do
              retryEvtAttempt re `shouldBe` 1
              retryEvtMaxAttempts re `shouldBe` 3
            _ -> expectationFailure "Expected Retry event"
          case evt2 of
            Retry re -> retryEvtAttempt re `shouldBe` 2
            _ -> expectationFailure "Expected Retry event"
        _ -> expectationFailure "Expected exactly 2 events"

  describe "withRetryPolicy" $ do
    it "overrides client retry policy" $ do
      let originalPolicy = RetryPolicy 3 (ExponentialBackoff 1.0 60.0)
          newPolicy = RetryPolicy 5 (ConstantBackoff 0.5)
          mockEnv = testEnv originalPolicy Nothing

      -- withRetryPolicy should pass env with modified policy to action
      let action env = do
            liftIO $ clientRetryPolicy env `shouldBe` newPolicy
            pure $ Right (42 :: Int)

      result <- withRetryPolicy newPolicy action mockEnv
      result `shouldBe` Right 42
