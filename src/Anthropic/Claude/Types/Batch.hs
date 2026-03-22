{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Batch
Description : Batch API types
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Types for the Claude Message Batches API, which processes
multiple message requests asynchronously.
-}
module Anthropic.Claude.Types.Batch
  ( -- * Batch Request
    BatchRequest(..)
  , CreateBatchRequest(..)

    -- * Batch Response
  , BatchResponse(..)
  , ProcessingStatus(..)
  , RequestCounts(..)

    -- * Batch Results
  , BatchResult(..)
  , BatchResultData(..)
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Request (CreateMessageRequest)
import Anthropic.Claude.Types.Response (MessageResponse)
import Anthropic.Claude.Types.Error (ErrorDetails)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | A single request within a batch.
data BatchRequest = BatchRequest
  { batchRequestCustomId :: Text
  , batchRequestParams :: CreateMessageRequest
  } deriving (Eq, Show, Generic)

instance FromJSON BatchRequest where
  parseJSON = withObject "BatchRequest" $ \obj ->
    BatchRequest <$> obj .: "custom_id" <*> obj .: "params"

instance ToJSON BatchRequest where
  toJSON (BatchRequest cid params) = object
    [ "custom_id" .= cid, "params" .= params ]

-- | Request to create a new batch.
data CreateBatchRequest = CreateBatchRequest
  { createBatchRequests :: [BatchRequest]
  } deriving (Eq, Show, Generic)

instance FromJSON CreateBatchRequest where
  parseJSON = withObject "CreateBatchRequest" $ \obj ->
    CreateBatchRequest <$> obj .: "requests"

instance ToJSON CreateBatchRequest where
  toJSON (CreateBatchRequest reqs) = object [ "requests" .= reqs ]

-- | Processing status of a batch.
data ProcessingStatus
  = InProgress
  | Canceling
  | Ended
  deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON ProcessingStatus where
  parseJSON = withText "ProcessingStatus" $ \case
    "in_progress" -> pure InProgress
    "canceling"   -> pure Canceling
    "ended"       -> pure Ended
    other         -> fail $ "Unknown ProcessingStatus: " <> T.unpack other

instance ToJSON ProcessingStatus where
  toJSON InProgress = "in_progress"
  toJSON Canceling  = "canceling"
  toJSON Ended      = "ended"

-- | Counts of requests in each state.
data RequestCounts = RequestCounts
  { requestCountsProcessing :: Int
  , requestCountsSucceeded :: Int
  , requestCountsErrored :: Int
  , requestCountsCanceled :: Int
  , requestCountsExpired :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON RequestCounts where
  parseJSON = genericParseJSON (prefixOptions "requestCounts")

instance ToJSON RequestCounts where
  toJSON = genericToJSON (prefixOptions "requestCounts")
  toEncoding = genericToEncoding (prefixOptions "requestCounts")

-- | Response from batch operations (create, retrieve, list).
data BatchResponse = BatchResponse
  { batchResponseId :: BatchId
  , batchResponseType :: Text
  , batchResponseProcessingStatus :: ProcessingStatus
  , batchResponseRequestCounts :: RequestCounts
  , batchResponseCreatedAt :: Text
  , batchResponseEndedAt :: Maybe Text
  , batchResponseResultsUrl :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" $ \obj ->
    BatchResponse
      <$> obj .: "id"
      <*> obj .: "type"
      <*> obj .: "processing_status"
      <*> obj .: "request_counts"
      <*> obj .: "created_at"
      <*> obj .:? "ended_at"
      <*> obj .:? "results_url"

instance ToJSON BatchResponse where
  toJSON (BatchResponse bid typ status counts created ended resultsUrl) = object $
    [ "id" .= bid
    , "type" .= typ
    , "processing_status" .= status
    , "request_counts" .= counts
    , "created_at" .= created
    ] ++ maybe [] (\e -> ["ended_at" .= e]) ended
      ++ maybe [] (\r -> ["results_url" .= r]) resultsUrl

-- | Result data for a single request in a completed batch.
data BatchResultData
  = SuccessResult MessageResponse
  | ErrorResult ErrorDetails
  | ExpiredResult
  deriving (Eq, Show, Generic)

instance FromJSON BatchResultData where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "succeeded" -> SuccessResult <$> obj .: "message"
    "errored"   -> ErrorResult <$> obj .: "error"
    "expired"   -> pure ExpiredResult
    other       -> fail $ "Unknown BatchResultData type: " <> T.unpack other

instance ToJSON BatchResultData where
  toJSON (SuccessResult msg) = object
    [ "type" .= ("succeeded" :: Text), "message" .= msg ]
  toJSON (ErrorResult err) = object
    [ "type" .= ("errored" :: Text), "error" .= err ]
  toJSON ExpiredResult = object
    [ "type" .= ("expired" :: Text) ]

-- | A single result from a completed batch (one per request).
data BatchResult = BatchResult
  { batchResultCustomId :: Text
  , batchResultResult :: BatchResultData
  } deriving (Eq, Show, Generic)

instance FromJSON BatchResult where
  parseJSON = withObject "BatchResult" $ \obj ->
    BatchResult <$> obj .: "custom_id" <*> obj .: "result"

instance ToJSON BatchResult where
  toJSON (BatchResult cid result) = object
    [ "custom_id" .= cid, "result" .= result ]
