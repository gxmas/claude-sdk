{-|
Module      : Anthropic.Claude.Internal.JSON
Description : Shared JSON encoding/decoding utilities
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Stability   : internal

Provides shared utilities for JSON serialization with snake_case/camelCase conversion
and discriminated union parsing. All other modules depend on these utilities.
-}
{-# LANGUAGE OverloadedStrings #-}
module Anthropic.Claude.Internal.JSON
  ( -- * Shared aeson options
    aesonOptions
  , aesonOptionsNoOmit
  , prefixOptions
    -- * Discriminated union parsing
  , withDiscriminator
    -- * Re-exports
  , module Data.Aeson
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text (Text)

-- | Standard aeson options for Claude API types.
--
-- **Warning**: Only use for types whose field names have no prefix.
-- For types with prefixed fields (e.g., @messageRole@, @toolName@),
-- use 'prefixOptions' instead.
aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  , constructorTagModifier = camelTo2 '_'
  }

-- | Aeson options that strip a record field prefix before converting
-- to snake_case.
--
-- Example:
-- @
-- data Message = Message { messageRole :: Role, messageContent :: Content }
--   deriving Generic
-- instance ToJSON Message where
--   toJSON = genericToJSON (prefixOptions "message")
-- -- Produces: {"role": "user", "content": "hello"}
-- @
prefixOptions :: String -> Options
prefixOptions prefix = aesonOptions
  { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix
  }
  where
    dropPrefix p s = case stripPrefix p s of
      Just (c:rest) -> toLower c : rest   -- lowercase the first char after prefix
      Just []       -> s                  -- prefix == field name, leave unchanged
      Nothing       -> camelTo2 '_' s     -- no prefix match, fall back to default

-- | Variant of 'aesonOptions' that does not omit Nothing fields.
--
-- Use for types where explicit null is meaningful (e.g., stop_sequence).
aesonOptionsNoOmit :: Options
aesonOptionsNoOmit = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = False
  , constructorTagModifier = camelTo2 '_'
  }

-- | Helper for parsing discriminated unions with a type field.
--
-- Extracts the discriminator field value and dispatches to a
-- variant-specific parser that receives both the type value and
-- the full JSON object.
--
-- Example:
-- @
-- instance FromJSON ContentBlock where
--   parseJSON = withDiscriminator "type" $ \\typeField o -> case typeField of
--     "text" -> TextBlock <$> o .: "text"
--     "image" -> ImageBlock <$> o .: "source"
--     other -> fail $ "Unknown ContentBlock type: " <> T.unpack other
-- @
withDiscriminator
  :: Text                         -- ^ Discriminator field name (e.g., "type")
  -> (Text -> Object -> Parser a) -- ^ Parser receiving type value and full object
  -> Value
  -> Parser a
withDiscriminator field dispatch = withObject "discriminated union" $ \o -> do
  typeField <- o .: fromText field
  dispatch typeField o
