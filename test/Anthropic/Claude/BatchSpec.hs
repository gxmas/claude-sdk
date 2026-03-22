{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.BatchSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "Batch" $ do

  describe "createBatch" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"

  describe "retrieveBatch" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"

  describe "listBatches" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"

  describe "cancelBatch" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"

  describe "pollBatchUntilDone" $ do
    it "placeholder - requires real API connection" $
      pendingWith "Integration tests require ANTHROPIC_API_KEY"
