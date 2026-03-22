{-|
Module      : Anthropic.Claude.Internal.JSON
Description : Shared JSON encoding/decoding utilities
Copyright   : (c) 2026 Anthropic
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
    -- * Discriminated union parsing
  , withDiscriminator
    -- * Re-exports
  , module Data.Aeson
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Parser)
import Data.Text (Text)

-- | Standard aeson options for Claude API types.
--
-- Conventions:
-- - Field names: camelCase (Haskell) → snake_case (JSON)
-- - Constructor tags: CamelCase → snake_case
-- - Optional fields: Omit Nothing values from JSON
--
-- Example:
-- @
-- data User = User { userName :: Text, userAge :: Maybe Int }
--   deriving Generic
-- instance ToJSON User where
--   toJSON = genericToJSON aesonOptions
-- -- Produces: {"user_name": "Alice", "user_age": 30} or {"user_name": "Alice"}
-- @
aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  , constructorTagModifier = camelTo2 '_'
  }

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
