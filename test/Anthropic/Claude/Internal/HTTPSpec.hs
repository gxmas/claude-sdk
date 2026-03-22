{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Internal.HTTPSpec (spec) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Error
import Network.HTTP.Types.Status (Status, mkStatus)
import Test.Hspec

spec :: Spec
spec = describe "Internal.HTTP" $ do

  describe "mkClientEnv" $ do
    it "creates a client environment with TLS manager" $ do
      env <- mkClientEnv (ApiKey "test-key")
      clientApiKey env `shouldBe` ApiKey "test-key"
      clientBaseUrl env `shouldBe` "https://api.anthropic.com"

  describe "extractRateLimitInfo" $ do
    it "returns Nothing for empty headers" $
      extractRateLimitInfo [] `shouldBe` Nothing

    it "extracts rate limit info from headers" $ do
      let headers =
            [ ("anthropic-ratelimit-requests-limit", "50")
            , ("anthropic-ratelimit-requests-remaining", "49")
            ]
      case extractRateLimitInfo headers of
        Just info -> do
          rateLimitRequests info `shouldBe` Just 50
          rateLimitRemaining info `shouldBe` Just 49
        Nothing -> expectationFailure "Expected rate limit info"

  describe "error kind mapping" $ do
    it "maps status codes correctly" $ do
      errorKindFromStatus (mkStatus 400 "") `shouldBe` InvalidRequestError
      errorKindFromStatus (mkStatus 401 "") `shouldBe` AuthenticationError
      errorKindFromStatus (mkStatus 429 "") `shouldBe` RateLimitError
      errorKindFromStatus (mkStatus 500 "") `shouldBe` ServerError
