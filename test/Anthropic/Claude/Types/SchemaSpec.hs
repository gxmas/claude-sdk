{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.SchemaSpec (spec) where

import Anthropic.Claude.Types.Schema
import Data.Aeson (Value(..), decode, encode)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary Instances

instance Arbitrary SchemaType where
  arbitrary = elements
    [StringType, NumberType, IntegerType, BooleanType, ArrayType, ObjectType, NullType]

instance Arbitrary TypeSpec where
  arbitrary = oneof
    [ SingleType <$> arbitrary
    , UnionType <$> listOf1 arbitrary
    ]

instance Arbitrary JsonSchema where
  arbitrary = sized genSchema
    where
      genSchema 0 = elements
        [ stringSchema, numberSchema, integerSchema, booleanSchema, nullSchema
        , emptySchema
        ]
      genSchema n = frequency
        -- Weight base cases heavily to keep trees small
        [ (5, pure stringSchema)
        , (5, pure numberSchema)
        , (5, pure integerSchema)
        , (5, pure booleanSchema)
        , (5, pure nullSchema)
        , (2, arraySchema <$> genSchema half)
        , (2, objectSchema <$> smallListOf1 (genProperty half))
        , (2, enumSchema <$> smallListOf1 genSimpleValue)
        , (1, oneOfSchema <$> smallListOf1 (genSchema half))
        , (1, anyOfSchema <$> smallListOf1 (genSchema half))
        , (1, allOfSchema <$> smallListOf1 (genSchema half))
        , (1, notSchema <$> genSchema half)
        , (2, refSchema <$> genText)
        , (2, do desc <- genText
                 s <- genSchema half
                 pure $ withDescription desc s)
        ]
        where half = n `div` 3

      -- Generate 1-3 elements (bounded, unlike listOf1 which is unbounded)
      smallListOf1 gen = do
        n <- choose (1, 3 :: Int)
        vectorOf n gen

      genProperty n = do
        name <- genText
        schema <- genSchema n
        isReq <- arbitrary
        pure $ Property name schema isReq

      genSimpleValue = oneof
        [ String <$> genText
        , Number <$> genScientific
        , Bool <$> arbitrary
        , pure Null
        ]

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a'..'z'])

genScientific :: Gen Scientific
genScientific = fromIntegral <$> (choose (-1000, 1000) :: Gen Int)

-- Tests

spec :: Spec
spec = describe "Types.Schema" $ do

  describe "SchemaType" $ do
    it "round-trips through JSON" $ property $
      \(st :: SchemaType) -> decode (encode st) === Just st

    it "encodes to expected strings" $ do
      encode StringType  `shouldBe` "\"string\""
      encode NumberType  `shouldBe` "\"number\""
      encode IntegerType `shouldBe` "\"integer\""
      encode BooleanType `shouldBe` "\"boolean\""
      encode ArrayType   `shouldBe` "\"array\""
      encode ObjectType  `shouldBe` "\"object\""
      encode NullType    `shouldBe` "\"null\""

  describe "TypeSpec" $ do
    it "round-trips through JSON" $ property $
      \(ts :: TypeSpec) -> decode (encode ts) === Just ts

    it "encodes SingleType as string" $ do
      encode (SingleType StringType) `shouldBe` "\"string\""

    it "encodes UnionType as array" $ do
      encode (UnionType [StringType, NullType]) `shouldBe` "[\"string\",\"null\"]"

    it "decodes string as SingleType" $ do
      decode "\"integer\"" `shouldBe` Just (SingleType IntegerType)

    it "decodes array as UnionType" $ do
      decode "[\"string\",\"null\"]" `shouldBe` Just (UnionType [StringType, NullType])

  describe "JsonSchema" $ do
    it "round-trips through JSON" $ property $
      \(s :: JsonSchema) -> decode (encode s) === Just s

    it "encodes emptySchema as empty object" $ do
      encode emptySchema `shouldBe` "{}"

    it "round-trips all 32 fields" $ do
      let fullSchema = JsonSchema
            { schemaType = Just (SingleType ObjectType)
            , schemaTitle = Just "Test"
            , schemaDescription = Just "A test schema"
            , schemaDefault = Just (String "default")
            , schemaExamples = Just [String "ex1", Number 42]
            , schemaDeprecated = Just True
            , schemaReadOnly = Just False
            , schemaWriteOnly = Just True
            , schemaEnum = Just [String "a", String "b"]
            , schemaConst = Just (String "fixed")
            , schemaProperties = Just $ Map.fromList
                [("name", stringSchema)]
            , schemaRequired = Just ["name"]
            , schemaAdditionalProperties = Just (Bool False)
            , schemaItems = Just stringSchema
            , schemaMinItems = Just 1
            , schemaMaxItems = Just 10
            , schemaUniqueItems = Just True
            , schemaMinimum = Just 0
            , schemaMaximum = Just 100
            , schemaExclusiveMinimum = Just (-1)
            , schemaExclusiveMaximum = Just 101
            , schemaMultipleOf = Just 5
            , schemaMinLength = Just 1
            , schemaMaxLength = Just 255
            , schemaPattern = Just "^[a-z]+$"
            , schemaFormat = Just "email"
            , schemaOneOf = Just [stringSchema, numberSchema]
            , schemaAnyOf = Just [integerSchema]
            , schemaAllOf = Just [booleanSchema]
            , schemaNot = Just nullSchema
            , schemaRef = Just "#/$defs/Address"
            , schemaDefs = Just $ Map.fromList
                [("Address", objectSchema [required "street" stringSchema])]
            }
      decode (encode fullSchema) `shouldBe` Just fullSchema

  describe "Primitive schemas" $ do
    it "stringSchema has type string" $
      schemaType stringSchema `shouldBe` Just (SingleType StringType)

    it "numberSchema has type number" $
      schemaType numberSchema `shouldBe` Just (SingleType NumberType)

    it "integerSchema has type integer" $
      schemaType integerSchema `shouldBe` Just (SingleType IntegerType)

    it "booleanSchema has type boolean" $
      schemaType booleanSchema `shouldBe` Just (SingleType BooleanType)

    it "nullSchema has type null" $
      schemaType nullSchema `shouldBe` Just (SingleType NullType)

  describe "objectSchema" $ do
    it "creates object with properties and required" $ do
      let s = objectSchema
            [ required "name" stringSchema
            , optional "age" integerSchema
            ]
      schemaType s `shouldBe` Just (SingleType ObjectType)
      schemaRequired s `shouldBe` Just ["name"]
      Map.size <$> schemaProperties s `shouldBe` Just 2

    it "omits required when all optional" $ do
      let s = objectSchema [optional "x" stringSchema]
      schemaRequired s `shouldBe` Nothing

  describe "arraySchema" $ do
    it "creates array with items" $ do
      let s = arraySchema stringSchema
      schemaType s `shouldBe` Just (SingleType ArrayType)
      schemaItems s `shouldBe` Just stringSchema

  describe "nullableSchema" $ do
    it "converts SingleType to UnionType with null" $ do
      let s = nullableSchema stringSchema
      schemaType s `shouldBe` Just (UnionType [StringType, NullType])

    it "appends null to existing UnionType" $ do
      let s = nullableSchema $ emptySchema
            { schemaType = Just (UnionType [StringType, IntegerType]) }
      schemaType s `shouldBe` Just (UnionType [StringType, IntegerType, NullType])

  describe "Composition combinators" $ do
    it "oneOfSchema sets oneOf" $ do
      let s = oneOfSchema [stringSchema, numberSchema]
      schemaOneOf s `shouldBe` Just [stringSchema, numberSchema]

    it "anyOfSchema sets anyOf" $ do
      let s = anyOfSchema [stringSchema]
      schemaAnyOf s `shouldBe` Just [stringSchema]

    it "allOfSchema sets allOf" $ do
      let s = allOfSchema [stringSchema, numberSchema]
      schemaAllOf s `shouldBe` Just [stringSchema, numberSchema]

    it "notSchema sets not" $ do
      let s = notSchema stringSchema
      schemaNot s `shouldBe` Just stringSchema

    it "refSchema sets $ref" $ do
      let s = refSchema "#/$defs/Foo"
      schemaRef s `shouldBe` Just "#/$defs/Foo"

  describe "Modifier combinators" $ do
    it "withDescription sets description" $ do
      let s = withDescription "test" stringSchema
      schemaDescription s `shouldBe` Just "test"

    it "withTitle sets title" $ do
      let s = withTitle "Title" stringSchema
      schemaTitle s `shouldBe` Just "Title"

    it "withFormat sets format" $ do
      let s = withFormat "email" stringSchema
      schemaFormat s `shouldBe` Just "email"

    it "withConst sets const" $ do
      let s = withConst (String "x") emptySchema
      schemaConst s `shouldBe` Just (String "x")

    it "withMinimum/withMaximum set bounds" $ do
      let s = withMinimum 0 $ withMaximum 100 numberSchema
      schemaMinimum s `shouldBe` Just 0
      schemaMaximum s `shouldBe` Just 100

    it "withExclusiveMinimum/withExclusiveMaximum set bounds" $ do
      let s = withExclusiveMinimum 0 $ withExclusiveMaximum 100 numberSchema
      schemaExclusiveMinimum s `shouldBe` Just 0
      schemaExclusiveMaximum s `shouldBe` Just 100

    it "withMultipleOf sets multipleOf" $ do
      let s = withMultipleOf 5 integerSchema
      schemaMultipleOf s `shouldBe` Just 5

    it "withMinLength/withMaxLength set bounds" $ do
      let s = withMinLength 1 $ withMaxLength 255 stringSchema
      schemaMinLength s `shouldBe` Just 1
      schemaMaxLength s `shouldBe` Just 255

    it "withPattern sets pattern" $ do
      let s = withPattern "^[a-z]+$" stringSchema
      schemaPattern s `shouldBe` Just "^[a-z]+$"

    it "withMinItems/withMaxItems set bounds" $ do
      let s = withMinItems 1 $ withMaxItems 10 $ arraySchema stringSchema
      schemaMinItems s `shouldBe` Just 1
      schemaMaxItems s `shouldBe` Just 10

    it "withUniqueItems sets uniqueItems" $ do
      let s = withUniqueItems $ arraySchema integerSchema
      schemaUniqueItems s `shouldBe` Just True

    it "withDefs sets $defs" $ do
      let defs = Map.fromList [("Foo", stringSchema)]
          s = withDefs defs emptySchema
      schemaDefs s `shouldBe` Just defs

    it "withExamples sets examples" $ do
      let s = withExamples [String "foo", Number 42] stringSchema
      schemaExamples s `shouldBe` Just [String "foo", Number 42]

  describe "JSON field names" $ do
    it "uses camelCase for JSON Schema fields" $ do
      let s = withMinLength 1 $ withMaxLength 10 stringSchema
          json = encode s
          jsonText = T.pack $ show json
      T.isInfixOf "minLength" jsonText `shouldBe` True
      T.isInfixOf "maxLength" jsonText `shouldBe` True
      -- Should NOT have snake_case
      T.isInfixOf "min_length" jsonText `shouldBe` False

    it "uses $ref and $defs for reference fields" $ do
      let s = refSchema "#/$defs/Foo"
          json = encode s
          jsonText = T.pack $ show json
      T.isInfixOf "$ref" jsonText `shouldBe` True
