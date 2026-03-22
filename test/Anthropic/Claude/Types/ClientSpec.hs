{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Anthropic.Claude.Types.ClientSpec (spec) where

import Anthropic.Claude.Types.Client
import Data.Aeson (decode, encode)
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Generators

instance Arbitrary BackoffStrategy where
  arbitrary = oneof
    [ ExponentialBackoff <$> genDelay <*> genDelay
    , ConstantBackoff <$> genDelay
    , pure NoBackoff
    ]
    where
      genDelay = fromIntegral <$> choose (1, 300 :: Int)

instance Arbitrary RetryPolicy where
  arbitrary = RetryPolicy
    <$> choose (0, 10)
    <*> arbitrary

instance Arbitrary RateLimitInfo where
  arbitrary = RateLimitInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- Test Suite

spec :: Spec
spec = describe "Types.Client" $ do

  describe "BackoffStrategy" $ do
    it "parses ExponentialBackoff from JSON" $ do
      let json = "{\"type\":\"exponential\",\"base\":1.0,\"max\":60.0}"
      case decode json of
        Just (ExponentialBackoff base maxDelay) -> do
          base `shouldBe` 1.0
          maxDelay `shouldBe` 60.0
        _ -> expectationFailure "Failed to parse ExponentialBackoff"

    it "parses ConstantBackoff from JSON" $ do
      let json = "{\"type\":\"constant\",\"delay\":5.0}"
      case decode json of
        Just (ConstantBackoff delay) -> delay `shouldBe` 5.0
        _ -> expectationFailure "Failed to parse ConstantBackoff"

    it "parses NoBackoff from JSON" $ do
      let json = "{\"type\":\"none\"}"
      decode json `shouldBe` Just NoBackoff

    it "encodes ExponentialBackoff with type field" $ do
      let strategy = ExponentialBackoff 1.0 60.0
      case decode (encode strategy) of
        Just (ExponentialBackoff _ _) -> pure ()
        _ -> expectationFailure "Failed to encode ExponentialBackoff"

    it "round-trips through JSON" $ property $
      \(strategy :: BackoffStrategy) -> decode (encode strategy) === Just strategy

  describe "RetryPolicy" $ do
    it "parses from JSON with snake_case fields" $ do
      let json = "{\"retry_max_attempts\":3,\"retry_strategy\":{\"type\":\"exponential\",\"base\":1.0,\"max\":60.0}}"
      case decode json of
        Just (RetryPolicy maxAttempts _) -> maxAttempts `shouldBe` 3
        _ -> expectationFailure "Failed to parse RetryPolicy"

    it "round-trips through JSON" $ property $
      \(policy :: RetryPolicy) -> decode (encode policy) === Just policy

  describe "RetryPolicy Presets" $ do
    it "defaultRetryPolicy has 3 max attempts" $
      retryMaxAttempts defaultRetryPolicy `shouldBe` 3

    it "defaultRetryPolicy uses exponential backoff" $
      case retryStrategy defaultRetryPolicy of
        ExponentialBackoff base maxDelay -> do
          base `shouldBe` 1.0
          maxDelay `shouldBe` 60.0
        _ -> expectationFailure "defaultRetryPolicy should use ExponentialBackoff"

    it "noRetries has 0 max attempts" $
      retryMaxAttempts noRetries `shouldBe` 0

    it "noRetries uses NoBackoff" $
      retryStrategy noRetries `shouldBe` NoBackoff

    it "aggressiveRetryPolicy has 5 max attempts" $
      retryMaxAttempts aggressiveRetryPolicy `shouldBe` 5

    it "aggressiveRetryPolicy uses exponential backoff" $
      case retryStrategy aggressiveRetryPolicy of
        ExponentialBackoff base maxDelay -> do
          base `shouldBe` 0.5
          maxDelay `shouldBe` 120.0
        _ -> expectationFailure "aggressiveRetryPolicy should use ExponentialBackoff"

  describe "RateLimitInfo" $ do
    it "parses from JSON with all fields" $ do
      let json = "{\"rate_limit_requests\":50,\"rate_limit_tokens\":40000,\"rate_limit_remaining\":49,\"rate_limit_tokens_remaining\":39000,\"rate_limit_reset_requests\":60,\"rate_limit_reset_tokens\":60}"
      case decode json of
        Just (RateLimitInfo reqs tokens remaining tokRem resetReq resetTok) -> do
          reqs `shouldBe` Just 50
          tokens `shouldBe` Just 40000
          remaining `shouldBe` Just 49
          tokRem `shouldBe` Just 39000
          resetReq `shouldBe` Just 60
          resetTok `shouldBe` Just 60
        _ -> expectationFailure "Failed to parse RateLimitInfo"

    it "parses from JSON with missing fields (all Maybe)" $ do
      let json = "{}"
      decode json `shouldBe` Just (RateLimitInfo Nothing Nothing Nothing Nothing Nothing Nothing)

    it "round-trips through JSON" $ property $
      \(info :: RateLimitInfo) -> decode (encode info) === Just info

    it "uses snake_case field names" $ do
      let info = RateLimitInfo (Just 50) (Just 40000) Nothing Nothing Nothing Nothing
      let encoded = show (encode info)
      encoded `shouldContain` "rate_limit_requests"
      encoded `shouldContain` "rate_limit_tokens"
