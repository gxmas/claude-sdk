{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Anthropic.Claude.Streaming
Description : Public streaming API for Claude
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

High-level streaming API with resource safety. Provides bracket-style
functions that ensure the HTTP connection is properly closed even if
an exception occurs.

Per ADR 0005, streams yield @Either APIError StreamEvent@ to allow
partial recovery from mid-stream errors. Use 'throwOnError' to
convert to a simpler stream that throws on error.
-}
module Anthropic.Claude.Streaming
  ( -- * Stream Type
    MessageStream

    -- * Resource-Safe Streaming
  , withMessageStream
  , forEachEvent

    -- * Stream Combinators
  , throwOnError

    -- * Exceptions
  , StreamException(..)
  ) where

import Anthropic.Claude.Internal.Streaming
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Stream
import Control.Exception (Exception, throwIO)
import qualified Network.HTTP.Client as HTTP
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import UnliftIO (MonadUnliftIO, MonadIO, liftIO, bracket)

-- | Execute a streaming request with automatic resource cleanup.
--
-- The callback receives a 'MessageStream' that yields
-- @Either APIError StreamEvent@ values. The HTTP connection is
-- guaranteed to be closed when the callback returns, even if
-- an exception is thrown.
--
-- Example:
-- @
-- withMessageStream env req $ \\stream -> do
--   finalMsg <- S.mapM_ (\\case
--     Left err  -> print err
--     Right evt -> handleEvent evt
--     ) stream
--   print (extractText finalMsg)
-- @
withMessageStream
  :: MonadUnliftIO m
  => ClientEnv
  -> CreateMessageRequest
  -> (MessageStream m -> m a)
  -> m a
withMessageStream env req callback =
  bracket
    (liftIO $ createMessageStream env req)
    (\(resp, _) -> liftIO $ HTTP.responseClose resp)
    (\(_, stream) -> callback stream)

-- | Consume all events with a callback, returning the final message.
--
-- A convenience wrapper over 'withMessageStream' that calls a function
-- for each @Right@ event and collects the assembled 'MessageResponse'.
--
-- Example:
-- @
-- finalMsg <- forEachEvent env req $ \\evt ->
--   case evt of
--     ContentBlockDelta payload ->
--       case contentBlockDeltaDelta payload of
--         TextDelta txt -> putStr (T.unpack txt)
--         _ -> pure ()
--     _ -> pure ()
-- putStrLn (\"\\nDone: \" ++ show (responseUsage finalMsg))
-- @
forEachEvent
  :: MonadUnliftIO m
  => ClientEnv
  -> CreateMessageRequest
  -> (StreamEvent -> m ())
  -> m MessageResponse
forEachEvent env req handler =
  withMessageStream env req $ \stream ->
    S.mapM_ handleItem stream
  where
    handleItem (Right evt) = handler evt
    handleItem (Left _)    = pure ()  -- errors are silently skipped

-- | Convert a stream of @Either e a@ into a stream of @a@,
-- throwing the first 'Left' as an exception.
--
-- Per ADR 0005, this is for the simple case where you don't need
-- partial error recovery.
--
-- Example:
-- @
-- withMessageStream env req $ \\stream -> do
--   let cleanStream = throwOnError stream
--   finalMsg <- S.mapM_ handleEvent cleanStream
--   print finalMsg
-- @
throwOnError
  :: MonadIO m
  => Stream (Of (Either APIError StreamEvent)) m r
  -> Stream (Of StreamEvent) m r
throwOnError = S.mapM $ \case
  Right evt -> pure evt
  Left err  -> liftIO $ throwIO (StreamException err)

-- | Wrapper to make APIError throwable from 'throwOnError'.
newtype StreamException = StreamException APIError
  deriving (Show)

instance Exception StreamException
