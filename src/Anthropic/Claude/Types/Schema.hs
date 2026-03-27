{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Schema
Description : JSON Schema types for tool input definitions
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Type-safe JSON Schema representation matching the official Anthropic
tool input_schema specification. Supports all standard JSON Schema
fields including composition (oneOf, anyOf, allOf, not), references
(\$ref, \$defs), and the full set of validation constraints.

The @type@ field supports both single types and union types:

@
{ "type": "string" }              -- SingleType StringType
{ "type": ["string", "null"] }    -- UnionType [StringType, NullType]
@

Build schemas using combinators:

@
let schema = objectSchema
      [ required "name" (withDescription "User name" stringSchema)
      , required "age"  (withMinimum 0 integerSchema)
      , optional "email" (withFormat "email" stringSchema)
      ]
@

= Advanced Examples

== Nullable Types

@
-- String that can be null
nullableSchema stringSchema  -- { "type": ["string", "null"] }
@

== Enums

@
enumSchema [String "red", String "green", String "blue"]
@

== Nested Objects

@
objectSchema
  [ required "person" $ objectSchema
      [ required "name" stringSchema
      , optional "age" integerSchema
      ]
  , required "tags" $ arraySchema stringSchema
  ]
@

== Union Types (oneOf)

@
oneOfSchema
  [ objectSchema [required "type" (enumSchema [String "email"])]
  , objectSchema [required "type" (enumSchema [String "phone"])]
  ]
@
-}
module Anthropic.Claude.Types.Schema
  ( -- * Schema Types
    SchemaType(..)
  , TypeSpec(..)
  , JsonSchema(..)
  , Property(..)

    -- * Empty Schema
  , emptySchema

    -- * Primitive Schemas
  , stringSchema
  , numberSchema
  , integerSchema
  , booleanSchema
  , nullSchema

    -- * Composite Schemas
  , objectSchema
  , arraySchema
  , nullableSchema
  , enumSchema
  , oneOfSchema
  , anyOfSchema
  , allOfSchema
  , notSchema
  , refSchema

    -- * Property Constructors
  , required
  , optional

    -- * Annotation Modifiers
  , withDescription
  , withTitle
  , withDefault
  , withExamples

    -- * Validation Modifiers
  , withConst
  , withFormat

    -- * Number Constraints
  , withMinimum
  , withMaximum
  , withExclusiveMinimum
  , withExclusiveMaximum
  , withMultipleOf

    -- * String Constraints
  , withMinLength
  , withMaxLength
  , withPattern

    -- * Array Constraints
  , withMinItems
  , withMaxItems
  , withUniqueItems

    -- * Reference / Definitions
  , withDefs
  ) where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T

-- | JSON Schema type keywords
data SchemaType
  = StringType
  | NumberType
  | IntegerType
  | BooleanType
  | ArrayType
  | ObjectType
  | NullType
  deriving (Eq, Show)

instance ToJSON SchemaType where
  toJSON = \case
    StringType  -> "string"
    NumberType  -> "number"
    IntegerType -> "integer"
    BooleanType -> "boolean"
    ArrayType   -> "array"
    ObjectType  -> "object"
    NullType    -> "null"

instance FromJSON SchemaType where
  parseJSON = withText "SchemaType" $ \case
    "string"  -> pure StringType
    "number"  -> pure NumberType
    "integer" -> pure IntegerType
    "boolean" -> pure BooleanType
    "array"   -> pure ArrayType
    "object"  -> pure ObjectType
    "null"    -> pure NullType
    other     -> fail $ "Unknown SchemaType: " <> T.unpack other

-- | Type specification: single type or union of types
--
-- JSON Schema allows @type@ to be a string or array:
--
-- @
-- { "type": "string" }              -- SingleType StringType
-- { "type": ["string", "null"] }    -- UnionType [StringType, NullType]
-- @
data TypeSpec
  = SingleType SchemaType
  | UnionType [SchemaType]
  deriving (Eq, Show)

instance ToJSON TypeSpec where
  toJSON (SingleType t) = toJSON t
  toJSON (UnionType ts) = toJSON ts

instance FromJSON TypeSpec where
  parseJSON v@(String _) = SingleType <$> parseJSON v
  parseJSON v@(Array _)  = UnionType <$> parseJSON v
  parseJSON _            = fail "TypeSpec: expected string or array of strings"

-- | JSON Schema definition
--
-- Supports the subset of JSON Schema used by Anthropic's tool input_schema.
-- All fields are optional; use combinators to build schemas incrementally.
data JsonSchema = JsonSchema
  { -- Type
    schemaType                 :: Maybe TypeSpec
    -- Annotations
  , schemaTitle                :: Maybe Text
  , schemaDescription          :: Maybe Text
  , schemaDefault              :: Maybe Value
  , schemaExamples             :: Maybe [Value]
  , schemaDeprecated           :: Maybe Bool
  , schemaReadOnly             :: Maybe Bool
  , schemaWriteOnly            :: Maybe Bool
    -- Validation
  , schemaEnum                 :: Maybe [Value]
  , schemaConst                :: Maybe Value
    -- Object
  , schemaProperties           :: Maybe (Map Text JsonSchema)
  , schemaRequired             :: Maybe [Text]
  , schemaAdditionalProperties :: Maybe Value
    -- Array
  , schemaItems                :: Maybe JsonSchema
  , schemaMinItems             :: Maybe Int
  , schemaMaxItems             :: Maybe Int
  , schemaUniqueItems          :: Maybe Bool
    -- Number
  , schemaMinimum              :: Maybe Scientific
  , schemaMaximum              :: Maybe Scientific
  , schemaExclusiveMinimum     :: Maybe Scientific
  , schemaExclusiveMaximum     :: Maybe Scientific
  , schemaMultipleOf           :: Maybe Scientific
    -- String
  , schemaMinLength            :: Maybe Int
  , schemaMaxLength            :: Maybe Int
  , schemaPattern              :: Maybe Text
  , schemaFormat               :: Maybe Text
    -- Composition
  , schemaOneOf                :: Maybe [JsonSchema]
  , schemaAnyOf                :: Maybe [JsonSchema]
  , schemaAllOf                :: Maybe [JsonSchema]
  , schemaNot                  :: Maybe JsonSchema
    -- References
  , schemaRef                  :: Maybe Text
  , schemaDefs                 :: Maybe (Map Text JsonSchema)
  } deriving (Eq, Show)

instance ToJSON JsonSchema where
  toJSON s = object $ catMaybes
    [ ("type" .=)                 <$> schemaType s
    , ("title" .=)                <$> schemaTitle s
    , ("description" .=)          <$> schemaDescription s
    , ("default" .=)              <$> schemaDefault s
    , ("examples" .=)             <$> schemaExamples s
    , ("deprecated" .=)           <$> schemaDeprecated s
    , ("readOnly" .=)             <$> schemaReadOnly s
    , ("writeOnly" .=)            <$> schemaWriteOnly s
    , ("enum" .=)                 <$> schemaEnum s
    , ("const" .=)                <$> schemaConst s
    , ("properties" .=)           <$> schemaProperties s
    , ("required" .=)             <$> schemaRequired s
    , ("additionalProperties" .=) <$> schemaAdditionalProperties s
    , ("items" .=)                <$> schemaItems s
    , ("minItems" .=)             <$> schemaMinItems s
    , ("maxItems" .=)             <$> schemaMaxItems s
    , ("uniqueItems" .=)          <$> schemaUniqueItems s
    , ("minimum" .=)              <$> schemaMinimum s
    , ("maximum" .=)              <$> schemaMaximum s
    , ("exclusiveMinimum" .=)     <$> schemaExclusiveMinimum s
    , ("exclusiveMaximum" .=)     <$> schemaExclusiveMaximum s
    , ("multipleOf" .=)           <$> schemaMultipleOf s
    , ("minLength" .=)            <$> schemaMinLength s
    , ("maxLength" .=)            <$> schemaMaxLength s
    , ("pattern" .=)              <$> schemaPattern s
    , ("format" .=)               <$> schemaFormat s
    , ("oneOf" .=)                <$> schemaOneOf s
    , ("anyOf" .=)                <$> schemaAnyOf s
    , ("allOf" .=)                <$> schemaAllOf s
    , ("not" .=)                  <$> schemaNot s
    , ("$ref" .=)                 <$> schemaRef s
    , ("$defs" .=)                <$> schemaDefs s
    ]

instance FromJSON JsonSchema where
  parseJSON = withObject "JsonSchema" $ \o -> JsonSchema
    <$> o .:? "type"
    <*> o .:? "title"
    <*> o .:? "description"
    <*> o .:? "default"
    <*> o .:? "examples"
    <*> o .:? "deprecated"
    <*> o .:? "readOnly"
    <*> o .:? "writeOnly"
    <*> o .:? "enum"
    <*> o .:? "const"
    <*> o .:? "properties"
    <*> o .:? "required"
    <*> o .:? "additionalProperties"
    <*> o .:? "items"
    <*> o .:? "minItems"
    <*> o .:? "maxItems"
    <*> o .:? "uniqueItems"
    <*> o .:? "minimum"
    <*> o .:? "maximum"
    <*> o .:? "exclusiveMinimum"
    <*> o .:? "exclusiveMaximum"
    <*> o .:? "multipleOf"
    <*> o .:? "minLength"
    <*> o .:? "maxLength"
    <*> o .:? "pattern"
    <*> o .:? "format"
    <*> o .:? "oneOf"
    <*> o .:? "anyOf"
    <*> o .:? "allOf"
    <*> o .:? "not"
    <*> o .:? "$ref"
    <*> o .:? "$defs"

-- | Property in an object schema
data Property = Property
  { propertyName       :: Text
  , propertySchema     :: JsonSchema
  , propertyIsRequired :: Bool
  } deriving (Eq, Show)

-- | A schema with all fields set to 'Nothing'
emptySchema :: JsonSchema
emptySchema = JsonSchema
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- --------------------------------------------------------------------------
-- Primitive Schemas
-- --------------------------------------------------------------------------

-- | @{ "type": "string" }@
stringSchema :: JsonSchema
stringSchema = emptySchema { schemaType = Just (SingleType StringType) }

-- | @{ "type": "number" }@
numberSchema :: JsonSchema
numberSchema = emptySchema { schemaType = Just (SingleType NumberType) }

-- | @{ "type": "integer" }@
integerSchema :: JsonSchema
integerSchema = emptySchema { schemaType = Just (SingleType IntegerType) }

-- | @{ "type": "boolean" }@
booleanSchema :: JsonSchema
booleanSchema = emptySchema { schemaType = Just (SingleType BooleanType) }

-- | @{ "type": "null" }@
nullSchema :: JsonSchema
nullSchema = emptySchema { schemaType = Just (SingleType NullType) }

-- --------------------------------------------------------------------------
-- Composite Schemas
-- --------------------------------------------------------------------------

-- | Build an object schema from a list of properties.
--
-- @
-- objectSchema
--   [ required "name" stringSchema
--   , optional "age" integerSchema
--   ]
-- -- { "type": "object", "properties": {...}, "required": ["name"] }
-- @
objectSchema :: [Property] -> JsonSchema
objectSchema props = emptySchema
  { schemaType = Just (SingleType ObjectType)
  , schemaProperties = Just $ Map.fromList
      [(propertyName p, propertySchema p) | p <- props]
  , schemaRequired = case [propertyName p | p <- props, propertyIsRequired p] of
      [] -> Nothing
      rs -> Just rs
  }

-- | @{ "type": "array", "items": ... }@
arraySchema :: JsonSchema -> JsonSchema
arraySchema items = emptySchema
  { schemaType = Just (SingleType ArrayType)
  , schemaItems = Just items
  }

-- | Make a schema nullable by adding @"null"@ to the type union.
--
-- @
-- nullableSchema stringSchema
-- -- { "type": ["string", "null"] }
-- @
nullableSchema :: JsonSchema -> JsonSchema
nullableSchema s = case schemaType s of
  Just (SingleType t) -> s { schemaType = Just (UnionType [t, NullType]) }
  Just (UnionType ts) -> s { schemaType = Just (UnionType (ts ++ [NullType])) }
  Nothing             -> s { schemaType = Just (SingleType NullType) }

-- | @{ "enum": [...] }@
enumSchema :: [Value] -> JsonSchema
enumSchema vals = emptySchema { schemaEnum = Just vals }

-- | @{ "oneOf": [...] }@
oneOfSchema :: [JsonSchema] -> JsonSchema
oneOfSchema schemas = emptySchema { schemaOneOf = Just schemas }

-- | @{ "anyOf": [...] }@
anyOfSchema :: [JsonSchema] -> JsonSchema
anyOfSchema schemas = emptySchema { schemaAnyOf = Just schemas }

-- | @{ "allOf": [...] }@
allOfSchema :: [JsonSchema] -> JsonSchema
allOfSchema schemas = emptySchema { schemaAllOf = Just schemas }

-- | @{ "not": ... }@
notSchema :: JsonSchema -> JsonSchema
notSchema s = emptySchema { schemaNot = Just s }

-- | @{ "$ref": "..." }@
refSchema :: Text -> JsonSchema
refSchema ref = emptySchema { schemaRef = Just ref }

-- --------------------------------------------------------------------------
-- Property Constructors
-- --------------------------------------------------------------------------

-- | A required property in an object schema
required :: Text -> JsonSchema -> Property
required name s = Property name s True

-- | An optional property in an object schema
optional :: Text -> JsonSchema -> Property
optional name s = Property name s False

-- --------------------------------------------------------------------------
-- Annotation Modifiers
-- --------------------------------------------------------------------------

-- | Set the @description@ field
withDescription :: Text -> JsonSchema -> JsonSchema
withDescription desc s = s { schemaDescription = Just desc }

-- | Set the @title@ field
withTitle :: Text -> JsonSchema -> JsonSchema
withTitle t s = s { schemaTitle = Just t }

-- | Set the @default@ field
withDefault :: Value -> JsonSchema -> JsonSchema
withDefault val s = s { schemaDefault = Just val }

-- | Set the @examples@ field
withExamples :: [Value] -> JsonSchema -> JsonSchema
withExamples es s = s { schemaExamples = Just es }

-- --------------------------------------------------------------------------
-- Validation Modifiers
-- --------------------------------------------------------------------------

-- | Set the @const@ field
withConst :: Value -> JsonSchema -> JsonSchema
withConst val s = s { schemaConst = Just val }

-- | Set the @format@ field (e.g., "email", "date-time", "uri")
withFormat :: Text -> JsonSchema -> JsonSchema
withFormat f s = s { schemaFormat = Just f }

-- --------------------------------------------------------------------------
-- Number Constraints
-- --------------------------------------------------------------------------

-- | Set @minimum@
withMinimum :: Scientific -> JsonSchema -> JsonSchema
withMinimum n s = s { schemaMinimum = Just n }

-- | Set @maximum@
withMaximum :: Scientific -> JsonSchema -> JsonSchema
withMaximum n s = s { schemaMaximum = Just n }

-- | Set @exclusiveMinimum@
withExclusiveMinimum :: Scientific -> JsonSchema -> JsonSchema
withExclusiveMinimum n s = s { schemaExclusiveMinimum = Just n }

-- | Set @exclusiveMaximum@
withExclusiveMaximum :: Scientific -> JsonSchema -> JsonSchema
withExclusiveMaximum n s = s { schemaExclusiveMaximum = Just n }

-- | Set @multipleOf@
withMultipleOf :: Scientific -> JsonSchema -> JsonSchema
withMultipleOf n s = s { schemaMultipleOf = Just n }

-- --------------------------------------------------------------------------
-- String Constraints
-- --------------------------------------------------------------------------

-- | Set @minLength@
withMinLength :: Int -> JsonSchema -> JsonSchema
withMinLength n s = s { schemaMinLength = Just n }

-- | Set @maxLength@
withMaxLength :: Int -> JsonSchema -> JsonSchema
withMaxLength n s = s { schemaMaxLength = Just n }

-- | Set @pattern@ (regex)
withPattern :: Text -> JsonSchema -> JsonSchema
withPattern p s = s { schemaPattern = Just p }

-- --------------------------------------------------------------------------
-- Array Constraints
-- --------------------------------------------------------------------------

-- | Set @minItems@
withMinItems :: Int -> JsonSchema -> JsonSchema
withMinItems n s = s { schemaMinItems = Just n }

-- | Set @maxItems@
withMaxItems :: Int -> JsonSchema -> JsonSchema
withMaxItems n s = s { schemaMaxItems = Just n }

-- | Set @uniqueItems@ to @true@
withUniqueItems :: JsonSchema -> JsonSchema
withUniqueItems s = s { schemaUniqueItems = Just True }

-- --------------------------------------------------------------------------
-- Reference / Definitions
-- --------------------------------------------------------------------------

-- | Set @$defs@ (schema definitions for use with @$ref@)
withDefs :: Map Text JsonSchema -> JsonSchema -> JsonSchema
withDefs defs s = s { schemaDefs = Just defs }
