{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Internal.SSE
Description : Server-Sent Events parser
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Stability   : internal

Parser for the Server-Sent Events (SSE) protocol as used by the
Claude streaming API.

SSE format:
@
event: message_start
data: {"type":"message_start","message":{...}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}

@

Rules:
- Lines starting with @data:@ carry event data
- Lines starting with @event:@ set the event type
- Lines starting with @:@ are comments (ignored)
- Empty lines delimit events
- Multiple @data:@ lines within one event are joined with newlines
-}
module Anthropic.Claude.Internal.SSE
  ( -- * SSE Types
    SSEEvent(..)
  , SSEField(..)

    -- * Parsing
  , parseSSELine
  , buildSSEEvents
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | A parsed SSE field from a single line.
data SSEField
  = DataField Text      -- ^ @data: ...@
  | EventField Text     -- ^ @event: ...@
  | RetryField Text     -- ^ @retry: ...@ (not used by Claude API)
  | IdField Text        -- ^ @id: ...@ (not used by Claude API)
  | CommentField Text   -- ^ @: ...@ (comment / keep-alive)
  deriving (Eq, Show)

-- | A complete SSE event assembled from one or more fields.
data SSEEvent = SSEEvent
  { sseEventType :: Maybe Text   -- ^ The @event:@ value, if present
  , sseEventData :: Text         -- ^ The @data:@ value(s), joined with newlines
  } deriving (Eq, Show)

-- | Parse a single line of SSE input into a field.
--
-- Returns 'Nothing' for empty lines (event delimiter).
-- Returns 'Just' a field for non-empty lines.
--
-- Examples:
--
-- >>> parseSSELine "data: hello"
-- Just (DataField "hello")
--
-- >>> parseSSELine "event: message_start"
-- Just (EventField "message_start")
--
-- >>> parseSSELine ": comment"
-- Just (CommentField " comment")
--
-- >>> parseSSELine ""
-- Nothing
parseSSELine :: BS.ByteString -> Maybe SSEField
parseSSELine line
  | BS.null line         = Nothing   -- empty line = event delimiter
  | BS8.head line == ':' = Just $ CommentField (decodeField (BS.drop 1 line))
  | otherwise            = case BS8.break (== ':') line of
      (field, rest)
        | BS.null rest  -> Just $ mkField field ""  -- field with no value
        | otherwise     ->
            let value = BS.drop 1 rest  -- drop the ':'
                -- strip optional leading space after ':'
                trimmed = if not (BS.null value) && BS8.head value == ' '
                          then BS.drop 1 value
                          else value
            in Just $ mkField field (decodeField trimmed)
  where
    decodeField = TE.decodeUtf8With (\_ _ -> Just '?')

    mkField "data"  v = DataField v
    mkField "event" v = EventField v
    mkField "retry" v = RetryField v
    mkField "id"    v = IdField v
    mkField _       _ = CommentField ""  -- unknown fields treated as comments

-- | Assemble a list of raw SSE lines into complete events.
--
-- Lines are grouped by empty-line delimiters. Within each group,
-- @data:@ fields are concatenated with newlines, and the last
-- @event:@ field (if any) sets the event type.
--
-- Comments and unknown fields are discarded.
buildSSEEvents :: [BS.ByteString] -> [SSEEvent]
buildSSEEvents = go Nothing []
  where
    go :: Maybe Text -> [Text] -> [BS.ByteString] -> [SSEEvent]
    go _ [] [] = []  -- No data, no input — done
    go eventType dataAcc [] =
      -- End of input — flush remaining accumulated data
      flush eventType dataAcc []
    go eventType dataAcc (line : rest) =
      case parseSSELine line of
        Nothing ->
          -- Empty line = event delimiter; emit event if we have data
          flush eventType dataAcc rest
        Just (DataField d) ->
          go eventType (dataAcc ++ [d]) rest
        Just (EventField e) ->
          go (Just e) dataAcc rest
        Just _ ->
          -- Comments, retry, id — skip
          go eventType dataAcc rest

    flush :: Maybe Text -> [Text] -> [BS.ByteString] -> [SSEEvent]
    flush _ [] rest = go Nothing [] rest
    flush eventType dataAcc rest =
      let event = SSEEvent eventType (T.intercalate "\n" dataAcc)
      in event : go Nothing [] rest
