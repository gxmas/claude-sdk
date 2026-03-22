{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.BatchSpec (spec) where

import Anthropic.Claude.Types.Batch
import Anthropic.Claude.Types.Core
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary ProcessingStatus where
  arbitrary = elements [InProgress, Canceling, Ended]

instance Arbitrary RequestCounts where
  arbitrary = RequestCounts
    <$> choose (0, 100) <*> choose (0, 100) <*> choose (0, 100)
    <*> choose (0, 100) <*> choose (0, 100)

instance Arbitrary BatchResponse where
  arbitrary = BatchResponse
    <$> (BatchId <$> genText)
    <*> pure "message_batch"
    <*> arbitrary
    <*> arbitrary
    <*> pure "2026-03-21T00:00:00Z"
    <*> pure Nothing
    <*> pure Nothing

-- Tests

spec :: Spec
spec = describe "Types.Batch" $ do

  describe "ProcessingStatus" $ do
    it "encodes InProgress as \"in_progress\"" $
      encode InProgress `shouldBe` "\"in_progress\""

    it "encodes Canceling as \"canceling\"" $
      encode Canceling `shouldBe` "\"canceling\""

    it "encodes Ended as \"ended\"" $
      encode Ended `shouldBe` "\"ended\""

    it "round-trips through JSON" $ property $
      \(s :: ProcessingStatus) -> decode (encode s) === Just s

  describe "RequestCounts" $ do
    it "round-trips through JSON" $ property $
      \(rc :: RequestCounts) -> decode (encode rc) === Just rc

  describe "BatchResponse" $ do
    it "round-trips through JSON" $ property $
      \(br :: BatchResponse) -> decode (encode br) === Just br

    it "parses from API response JSON" $ do
      let json = "{\"id\":\"batch_123\",\"type\":\"message_batch\",\"processing_status\":\"in_progress\",\"request_counts\":{\"processing\":5,\"succeeded\":0,\"errored\":0,\"canceled\":0,\"expired\":0},\"created_at\":\"2026-03-21\"}"
      case decode json of
        Just (br :: BatchResponse) -> do
          batchResponseId br `shouldBe` BatchId "batch_123"
          batchResponseProcessingStatus br `shouldBe` InProgress
        Nothing -> expectationFailure "Failed to parse BatchResponse"

  describe "BatchResultData" $ do
    it "parses expired result" $ do
      let json = "{\"type\":\"expired\"}"
      decode json `shouldBe` Just ExpiredResult

    it "round-trips ExpiredResult" $
      decode (encode ExpiredResult) `shouldBe` Just ExpiredResult

  describe "BatchResult" $ do
    it "parses result with custom_id" $ do
      let json = "{\"custom_id\":\"req-1\",\"result\":{\"type\":\"expired\"}}"
      case decode json of
        Just (br :: BatchResult) -> do
          batchResultCustomId br `shouldBe` "req-1"
          batchResultResult br `shouldBe` ExpiredResult
        Nothing -> expectationFailure "Failed to parse BatchResult"
