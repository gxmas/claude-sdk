{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Anthropic.Claude.StreamingSpec (spec) where

import Anthropic.Claude.Streaming
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Stream
import Anthropic.Claude.Internal.Streaming (defaultMessageResponse)
import Control.Exception (try, SomeException)
import qualified Streaming.Prelude as S
import Test.Hspec

spec :: Spec
spec = describe "Streaming" $ do

  describe "throwOnError" $ do
    it "passes Right values through" $ do
      let stream = do
            S.yield (Right Ping)
            S.yield (Right MessageStop)
            pure defaultMessageResponse
          cleanStream = throwOnError stream
      events <- S.toList_ cleanStream
      events `shouldBe` [Ping, MessageStop]

    it "throws on Left value" $ do
      let err = APIError ServerError (ErrorDetails "api_error" "fail") 500
          stream = do
            S.yield (Left err)
            pure defaultMessageResponse
          cleanStream = throwOnError stream
      result <- try (S.toList_ cleanStream)
      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> expectationFailure "Expected exception"

  describe "forEachEvent" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"

  describe "withMessageStream" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"
