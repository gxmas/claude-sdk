{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.ErrorSpec (spec) where

import Anthropic.Claude.Types.Error
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Generators

instance Arbitrary ErrorDetails where
  arbitrary =
    ErrorDetails
      <$> elements errorTypes
      <*> genText
   where
    errorTypes =
      [ "invalid_request_error"
      , "authentication_error"
      , "permission_error"
      , "not_found_error"
      , "rate_limit_error"
      , "api_error"
      , "overloaded_error"
      ]
    genText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary APIErrorKind where
  arbitrary =
    elements
      [ InvalidRequestError
      , AuthenticationError
      , PermissionError
      , NotFoundError
      , RateLimitError
      , ServerError
      , OverloadedError
      ]

instance Arbitrary APIError where
  arbitrary = do
    kind <- arbitrary
    let typeString = case kind of
          InvalidRequestError -> "invalid_request_error"
          AuthenticationError -> "authentication_error"
          PermissionError -> "permission_error"
          NotFoundError -> "not_found_error"
          RateLimitError -> "rate_limit_error"
          ServerError -> "api_error"
          OverloadedError -> "overloaded_error"
    msg <- T.pack <$> listOf1 (elements ['a' .. 'z'])
    let details = ErrorDetails typeString msg
    status <- choose (400, 529)
    pure $ APIError kind details status

instance Arbitrary NetworkError where
  arbitrary =
    oneof
      [ ConnectionError <$> genText
      , TimeoutError <$> genText
      , TLSError <$> genText
      , DNSError <$> genText
      , UnknownNetworkError <$> genText
      ]
   where
    genText = T.pack <$> listOf1 (elements ['a' .. 'z'])

-- Test Suite

spec :: Spec
spec = describe "Types.Error" $ do
  describe "ErrorDetails" $ do
    it "parses from JSON" $ do
      let json = "{\"error\":{\"type\":\"invalid_request_error\",\"message\":\"Invalid parameter\"}}"
      let expected = ErrorDetails "invalid_request_error" "Invalid parameter"
      decode json `shouldBe` Just expected

    it "round-trips through JSON"
      $ property
      $ \(details :: ErrorDetails) -> decode (encode details) === Just details

  describe "APIErrorKind" $ do
    it "encodes InvalidRequestError as \"invalid_request_error\""
      $ encode InvalidRequestError `shouldBe` "\"invalid_request_error\""

    it "encodes AuthenticationError as \"authentication_error\""
      $ encode AuthenticationError `shouldBe` "\"authentication_error\""

    it "encodes PermissionError as \"permission_error\""
      $ encode PermissionError `shouldBe` "\"permission_error\""

    it "encodes NotFoundError as \"not_found_error\""
      $ encode NotFoundError `shouldBe` "\"not_found_error\""

    it "encodes RateLimitError as \"rate_limit_error\""
      $ encode RateLimitError `shouldBe` "\"rate_limit_error\""

    it "encodes ServerError as \"api_error\""
      $ encode ServerError `shouldBe` "\"api_error\""

    it "encodes OverloadedError as \"overloaded_error\""
      $ encode OverloadedError `shouldBe` "\"overloaded_error\""

    it "round-trips through JSON"
      $ property
      $ \(kind :: APIErrorKind) -> decode (encode kind) === Just kind

  describe "APIError" $ do
    it "parses from API error response" $ do
      let json = "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"},\"status\":429}"
      case decode json of
        Just (APIError kind details status) -> do
          kind `shouldBe` RateLimitError
          errorMessage details `shouldBe` "Rate limit exceeded"
          status `shouldBe` 429
        Nothing -> expectationFailure "Failed to parse APIError"

    it "round-trips through JSON"
      $ property
      $ \(err :: APIError) -> decode (encode err) === Just err

  describe "isRetryable" $ do
    it "returns True for RateLimitError" $ do
      let err = APIError RateLimitError (ErrorDetails "rate_limit_error" "msg") 429
      isRetryable err `shouldBe` True

    it "returns True for ServerError" $ do
      let err = APIError ServerError (ErrorDetails "api_error" "msg") 500
      isRetryable err `shouldBe` True

    it "returns True for OverloadedError" $ do
      let err = APIError OverloadedError (ErrorDetails "overloaded_error" "msg") 529
      isRetryable err `shouldBe` True

    it "returns False for InvalidRequestError" $ do
      let err = APIError InvalidRequestError (ErrorDetails "invalid_request_error" "msg") 400
      isRetryable err `shouldBe` False

    it "returns False for AuthenticationError" $ do
      let err = APIError AuthenticationError (ErrorDetails "authentication_error" "msg") 401
      isRetryable err `shouldBe` False

    it "returns False for PermissionError" $ do
      let err = APIError PermissionError (ErrorDetails "permission_error" "msg") 403
      isRetryable err `shouldBe` False

    it "returns False for NotFoundError" $ do
      let err = APIError NotFoundError (ErrorDetails "not_found_error" "msg") 404
      isRetryable err `shouldBe` False

  describe "errorKindFromStatus" $ do
    it "maps 400 to InvalidRequestError"
      $ errorKindFromStatus status400 `shouldBe` InvalidRequestError

    it "maps 401 to AuthenticationError"
      $ errorKindFromStatus status401 `shouldBe` AuthenticationError

    it "maps 403 to PermissionError"
      $ errorKindFromStatus status403 `shouldBe` PermissionError

    it "maps 404 to NotFoundError"
      $ errorKindFromStatus status404 `shouldBe` NotFoundError

    it "maps 429 to RateLimitError"
      $ errorKindFromStatus status429 `shouldBe` RateLimitError

    it "maps 500 to ServerError"
      $ errorKindFromStatus status500 `shouldBe` ServerError

    it "maps 529 to OverloadedError"
      $ errorKindFromStatus (Status 529 "") `shouldBe` OverloadedError

    it "maps 5xx codes to ServerError"
      $ errorKindFromStatus (Status 503 "") `shouldBe` ServerError

  describe "NetworkError" $ do
    it "shows ConnectionError correctly" $ do
      let err = ConnectionError "connection refused"
      show err `shouldContain` "ConnectionError"
      show err `shouldContain` "connection refused"

    it "shows TimeoutError correctly" $ do
      let err = TimeoutError "timeout after 30s"
      show err `shouldContain` "TimeoutError"
