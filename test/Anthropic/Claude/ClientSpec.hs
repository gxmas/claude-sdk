{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Anthropic.Claude.ClientSpec (spec) where

import Anthropic.Claude.Client
import Anthropic.Claude.Types.Core
import Test.Hspec
import UnliftIO (liftIO)

spec :: Spec
spec = describe "Client" $ do
  describe "Retry Policy Presets" $ do
    it "defaultRetryPolicy has correct configuration" $ do
      retryMaxAttempts defaultRetryPolicy `shouldBe` 3
      case retryStrategy defaultRetryPolicy of
        ExponentialBackoff base maxDelay -> do
          base `shouldBe` 1.0
          maxDelay `shouldBe` 60.0
        _ -> expectationFailure "Expected ExponentialBackoff"

    it "noRetries has zero max attempts" $ do
      retryMaxAttempts noRetries `shouldBe` 0
      retryStrategy noRetries `shouldBe` NoBackoff

    it "aggressiveRetryPolicy has correct configuration" $ do
      retryMaxAttempts aggressiveRetryPolicy `shouldBe` 5
      case retryStrategy aggressiveRetryPolicy of
        ExponentialBackoff base maxDelay -> do
          base `shouldBe` 0.5
          maxDelay `shouldBe` 120.0
        _ -> expectationFailure "Expected ExponentialBackoff"

  describe "withRetryPolicy" $ do
    it "modifies client environment retry policy" $ do
      let originalPolicy = defaultRetryPolicy
          customPolicy = noRetries
          mockEnv =
            ClientEnv
              { clientApiKey = ApiKey "test-key"
              , clientRetryPolicy = originalPolicy
              , clientBaseUrl = "https://api.anthropic.com"
              , clientManager = undefined
              , clientEventHandler = Nothing
              , clientLogSettings = Nothing
              }

      let action env = do
            liftIO $ clientRetryPolicy env `shouldBe` customPolicy
            pure $ Right ("success" :: String)

      result <- withRetryPolicy customPolicy action mockEnv
      result `shouldBe` Right "success"

  describe "mkClientEnv" $ do
    it "creates client with default retry policy" $ do
      env <- mkClientEnv (ApiKey "test-key")
      clientApiKey env `shouldBe` ApiKey "test-key"
      clientRetryPolicy env `shouldBe` defaultRetryPolicy
      clientBaseUrl env `shouldBe` "https://api.anthropic.com"
