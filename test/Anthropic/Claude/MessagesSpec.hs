{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.MessagesSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Messages" $ do

  describe "createMessage" $ do
    it "placeholder - requires real API key for integration tests" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY environment variable"

    -- Note: Full integration tests would be added here
    -- They would require a real API key and would test:
    -- - Successful 200 response parsing
    -- - Error response handling (400, 401, 429, 500)
    -- - Rate limit header extraction
    -- - Request ID preservation
    -- - Retry behavior on transient errors
