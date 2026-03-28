{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.MessagesSpec (spec) where

import Anthropic.Claude.Internal.HTTP (extractRateLimitInfo, parseResponse)
import Anthropic.Claude.TestHelpers
import Anthropic.Claude.Types.Client (RateLimitInfo (..))
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Response
import Test.Hspec

spec :: Spec
spec = describe "Messages" $ do
  describe "createMessage (parseResponse)" $ do
    it "parses successful 200 response into MessageResponse" $ do
      let resp = mockSuccess sampleMessageResponseJson
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Right msg -> do
          responseId msg `shouldBe` MessageId "msg_test_123"
          extractText msg `shouldBe` "Hello! How can I help you?"
          usageInputTokens (responseUsage msg) `shouldBe` 15
          usageOutputTokens (responseUsage msg) `shouldBe` 8
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "returns InvalidRequestError for 400" $ do
      let resp = mockError 400 "invalid_request_error" "max_tokens must be positive"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> do
          errorKind err `shouldBe` InvalidRequestError
          errorStatusCode err `shouldBe` 400
        Right _ -> expectationFailure "Expected error"

    it "returns AuthenticationError for 401" $ do
      let resp = mockError 401 "authentication_error" "Invalid API key"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> errorKind err `shouldBe` AuthenticationError
        Right _ -> expectationFailure "Expected error"

    it "returns RateLimitError for 429" $ do
      let resp = mockError 429 "rate_limit_error" "Rate limit exceeded"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> do
          errorKind err `shouldBe` RateLimitError
          isRetryable err `shouldBe` True
        Right _ -> expectationFailure "Expected error"

    it "returns ServerError for 500" $ do
      let resp = mockError 500 "api_error" "Internal server error"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> do
          errorKind err `shouldBe` ServerError
          isRetryable err `shouldBe` True
        Right _ -> expectationFailure "Expected error"

    it "returns OverloadedError for 529" $ do
      let resp = mockError 529 "overloaded_error" "Overloaded"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> do
          errorKind err `shouldBe` OverloadedError
          isRetryable err `shouldBe` True
        Right _ -> expectationFailure "Expected error"

    it "handles unparseable error bodies" $ do
      let resp = mkMockResponse 500 [] "<html>Server Error</html>"
      case parseResponse resp Nothing :: Either APIError MessageResponse of
        Left err -> do
          errorKind err `shouldBe` ServerError
          errorMessage (errorDetails err) `shouldBe` "Failed to parse error response"
        Right _ -> expectationFailure "Expected error"

  describe "countTokens (parseResponse)" $ do
    it "parses successful 200 token count response" $ do
      let resp = mockSuccess sampleTokenCountJson
      case parseResponse resp Nothing :: Either APIError TokenCount of
        Right tc -> tokenCountInputTokens tc `shouldBe` 14
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "returns error for 400 response" $ do
      let resp = mockError 400 "invalid_request_error" "messages must not be empty"
      case parseResponse resp Nothing :: Either APIError TokenCount of
        Left err -> errorKind err `shouldBe` InvalidRequestError
        Right _ -> expectationFailure "Expected error"

  describe "extractRateLimitInfo" $ do
    it "extracts rate limit info from response headers" $ do
      let headers =
            [ ("anthropic-ratelimit-requests-limit", "50")
            , ("anthropic-ratelimit-requests-remaining", "49")
            ]
      case extractRateLimitInfo headers of
        Just info -> do
          rateLimitRequests info `shouldBe` Just 50
          rateLimitRemaining info `shouldBe` Just 49
        Nothing -> expectationFailure "Expected rate limit info"
