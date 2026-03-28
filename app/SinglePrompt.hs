{-# LANGUAGE OverloadedStrings #-}

{- |
Single-prompt executable for testing the Claude SDK.

Reads ANTHROPIC_API_KEY from environment, prompts user for input,
sends to Claude API, and prints the response or error.

Usage:

@
single-prompt [OPTIONS]
  --system TEXT           System prompt
  --max-tokens INT       Maximum tokens to generate (default: 1024)
  --stream               Enable streaming via server-sent events
  --temperature DOUBLE   Sampling temperature, 0.0 to 1.0 (default: 1.0)
  --top-k INT            Top-k sampling parameter
  --top-p DOUBLE         Nucleus sampling parameter, 0.0 to 1.0
  --thinking INT         Extended thinking budget in tokens (must be >= 1024)
  --service-tier TEXT    Service tier (e.g. "standard", "priority")
@
-}
module Main (main) where

import Anthropic.Claude.Client (mkClientEnv, withLogger)
import Anthropic.Claude.Messages
import Anthropic.Claude.Types hiding (optional)
import Control.Monad (when)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hFlush, stdout)

-- | Command-line options for the single-prompt executable
data Options = Options
  { optModel :: ModelId
  , optSystem :: Maybe T.Text
  , optMaxTokens :: Int
  , optStream :: Bool
  , optTemperature :: Maybe Double
  , optTopK :: Maybe Int
  , optTopP :: Maybe Double
  , optThinking :: Maybe Int
  , optServiceTier :: Maybe T.Text
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      validateModelId
      ( long "model"
          <> metavar "MODEL"
          <> value claude4Sonnet
          <> showDefaultWith (T.unpack . unModelId)
          <> help
            "Model to use: claude-opus-4-6, claude-sonnet-4-6, \
            \claude-haiku-4-5-20251001."
      )
    <*> optional
      ( strOption
          ( long "system"
              <> metavar "TEXT"
              <> help
                "System prompt. Provides context and instructions to Claude, \
                \such as specifying a particular goal or role."
          )
      )
    <*> option
      auto
      ( long "max-tokens"
          <> metavar "INT"
          <> value 1024
          <> showDefault
          <> help
            "Maximum number of tokens to generate before stopping. \
            \The model may stop before reaching this maximum."
      )
    <*> switch
      ( long "stream"
          <> help
            "Incrementally stream the response using server-sent events."
      )
    <*> optional
      ( option
          validateTemperature
          ( long "temperature"
              <> metavar "DOUBLE"
              <> help
                "Amount of randomness injected into the response (0.0 to 1.0, \
                \default: 1.0). Use closer to 0.0 for analytical tasks, \
                \closer to 1.0 for creative tasks."
          )
      )
    <*> optional
      ( option
          auto
          ( long "top-k"
              <> metavar "INT"
              <> help
                "Only sample from the top K options for each subsequent token. \
                \Removes long-tail low probability responses. \
                \Recommended for advanced use cases only."
          )
      )
    <*> optional
      ( option
          validateTopP
          ( long "top-p"
              <> metavar "DOUBLE"
              <> help
                "Nucleus sampling parameter (0.0 to 1.0). Cuts off the \
                \cumulative distribution at the specified probability. \
                \Alter temperature or top-p, but not both. \
                \Recommended for advanced use cases only."
          )
      )
    <*> optional
      ( option
          validateThinking
          ( long "thinking"
              <> metavar "INT"
              <> help
                "Extended thinking budget in tokens. Controls how many tokens \
                \Claude can use for internal reasoning. Must be >= 1024 and \
                \less than max-tokens."
          )
      )
    <*> optional
      ( strOption
          ( long "service-tier"
              <> metavar "TEXT"
              <> help
                "Service tier for this request (e.g. \"standard\", \"priority\"). \
                \Determines whether to use priority or standard capacity."
          )
      )

-- | Validate model is a known model ID
validateModelId :: ReadM ModelId
validateModelId = do
  s <- str
  let model = ModelId (T.pack s)
  if model `elem` [claude4Opus, claude4Sonnet, claude35Haiku]
    then pure model
    else
      readerError $
        "unknown model: "
          ++ s
          ++ ". Must be one of: claude-opus-4-6, claude-sonnet-4-6, claude-haiku-4-5-20251001"

-- | Validate temperature is in [0.0, 1.0]
validateTemperature :: ReadM Double
validateTemperature = do
  t <- auto
  if t < 0.0 || t > 1.0
    then readerError "temperature must be between 0.0 and 1.0"
    else pure t

-- | Validate top-p is in [0.0, 1.0]
validateTopP :: ReadM Double
validateTopP = do
  p <- auto
  if p < 0.0 || p > 1.0
    then readerError "top-p must be between 0.0 and 1.0"
    else pure p

-- | Validate thinking budget is >= 1024
validateThinking :: ReadM Int
validateThinking = do
  b <- auto
  if b < 1024
    then readerError "thinking budget must be >= 1024"
    else pure b

-- | Build a CreateMessageRequest from CLI options and user prompt
buildRequest :: Options -> T.Text -> CreateMessageRequest
buildRequest opts prompt =
  mkRequest (optModel opts) [userMsg prompt] (optMaxTokens opts)
    & maybe id (withSystem . SystemText) (optSystem opts)
    & applyIf (optStream opts) withStreaming
    & maybe id withTemperature (optTemperature opts)
    & maybe id withTopK (optTopK opts)
    & maybe id withTopP (optTopP opts)
    & maybe id (withThinking . ThinkingEnabled) (optThinking opts)
    & maybe id withServiceTier (optServiceTier opts)
  where
    applyIf True f = f
    applyIf False _ = id

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Send a single prompt to the Claude API"
            <> header "single-prompt - Claude SDK example"
        )

  -- Get API key from environment
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  apiKey <- case maybeKey of
    Nothing -> die "Error: ANTHROPIC_API_KEY environment variable not set"
    Just key -> pure $ ApiKey (T.pack key)

  -- Create client environment with debug logging
  env <- withLogger debugLogger <$> mkClientEnv apiKey

  -- Get prompt from user
  putStr "Enter your prompt: "
  hFlush stdout
  prompt <- TIO.getLine

  -- Validate prompt
  when (T.null prompt) $ die "Error: Prompt cannot be empty"

  -- Create request
  let request = buildRequest opts prompt

  -- Send request
  putStrLn "\nSending request to Claude API..."
  result <- createMessage env request

  -- Handle response
  case result of
    Left err -> do
      putStrLn "\n\x274c API Error:"
      putStrLn $ "  Kind: " ++ show (errorKind err)
      putStrLn $ "  Message: " ++ T.unpack (errorMessage $ errorDetails err)
      putStrLn $ "  Status Code: " ++ show (errorStatusCode err)
    Right response -> do
      let message = apiResponseBody response
          text = extractText message
          usage = responseUsage message

      putStrLn "\n\x2705 Response from Claude:\n"
      TIO.putStrLn text

      putStrLn "\nUsage:"
      putStrLn $ "  Input tokens: " ++ show (usageInputTokens usage)
      putStrLn $ "  Output tokens: " ++ show (usageOutputTokens usage)

      -- Show rate limit info if available
      case apiResponseRateLimitInfo response of
        Nothing -> pure ()
        Just rateLimit -> do
          putStrLn "\nRate Limit Info:"
          case rateLimitRemaining rateLimit of
            Just remaining -> putStrLn $ "  Requests remaining: " ++ show remaining
            Nothing -> pure ()
          case rateLimitTokensRemaining rateLimit of
            Just tokensRemaining -> putStrLn $ "  Tokens remaining: " ++ show tokensRemaining
            Nothing -> pure ()
