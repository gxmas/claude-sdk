{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.BatchSpec (spec) where

import Anthropic.Claude.Internal.HTTP (parseResponse)
import Anthropic.Claude.TestHelpers
import Anthropic.Claude.Types.Batch
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Test.Hspec

spec :: Spec
spec = describe "Batch" $ do

  describe "createBatch (parseResponse)" $ do
    it "parses successful 200 batch response" $ do
      let resp = mockSuccess sampleBatchResponseJson
      case parseResponse resp Nothing :: Either APIError BatchResponse of
        Right batch -> do
          batchResponseId batch `shouldBe` BatchId "batch_test_456"
          batchResponseProcessingStatus batch `shouldBe` InProgress
          requestCountsProcessing (batchResponseRequestCounts batch) `shouldBe` 5
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "returns error for 400 response" $ do
      let resp = mockError 400 "invalid_request_error" "Invalid batch request"
      case parseResponse resp Nothing :: Either APIError BatchResponse of
        Left err -> errorKind err `shouldBe` InvalidRequestError
        Right _ -> expectationFailure "Expected error"

  describe "retrieveBatch (parseResponse)" $ do
    it "parses ended batch response" $ do
      let resp = mockSuccess sampleBatchEndedJson
      case parseResponse resp Nothing :: Either APIError BatchResponse of
        Right batch -> do
          batchResponseProcessingStatus batch `shouldBe` Ended
          requestCountsSucceeded (batchResponseRequestCounts batch) `shouldBe` 5
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "listBatches (parseResponse)" $ do
    it "parses list of batch responses" $ do
      let listJson = "[" <> sampleBatchResponseJson <> "]"
          resp = mockSuccess listJson
      case parseResponse resp Nothing :: Either APIError [BatchResponse] of
        Right batches -> length batches `shouldBe` 1
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "cancelBatch (parseResponse)" $ do
    it "parses canceling batch response" $ do
      let cancelingJson = "{\"id\":\"batch_test_456\",\"type\":\"message_batch\",\"processing_status\":\"canceling\",\"request_counts\":{\"processing\":3,\"succeeded\":2,\"errored\":0,\"canceled\":0,\"expired\":0},\"created_at\":\"2026-03-21T00:00:00Z\"}"
          resp = mockSuccess cancelingJson
      case parseResponse resp Nothing :: Either APIError BatchResponse of
        Right batch -> batchResponseProcessingStatus batch `shouldBe` Canceling
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "pollBatchUntilDone" $ do
    it "detects ended status" $ do
      let resp = mockSuccess sampleBatchEndedJson
      case parseResponse resp Nothing :: Either APIError BatchResponse of
        Right batch -> batchResponseProcessingStatus batch `shouldBe` Ended
        Left err -> expectationFailure $ "Expected success, got: " ++ show err
