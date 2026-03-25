{-# LANGUAGE OverloadedStrings #-}

-- |
-- Single-prompt executable for testing the Claude SDK.
--
-- Reads ANTHROPIC_API_KEY from environment, prompts user for input,
-- sends to Claude API, and prints the response or error.
module Main (main) where

import Anthropic.Claude.Client (mkClientEnv, withLogger)
import Anthropic.Claude.Messages
import Anthropic.Claude.Types
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
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
  let request = mkRequest claude4Sonnet [userMsg prompt] 1024

  -- Send request
  putStrLn "\nSending request to Claude API..."
  result <- createMessage env request

  -- Handle response
  case result of
    Left err -> do
      putStrLn "\n❌ API Error:"
      putStrLn $ "  Kind: " ++ show (errorKind err)
      putStrLn $ "  Message: " ++ T.unpack (errorMessage $ errorDetails err)
      putStrLn $ "  Status Code: " ++ show (errorStatusCode err)
    Right response -> do
      let message = apiResponseBody response
          text = extractText message
          usage = responseUsage message

      putStrLn "\n✅ Response from Claude:\n"
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
