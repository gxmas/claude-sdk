{-# LANGUAGE OverloadedStrings #-}

{- |
Streaming prompt example for the Claude SDK.

Reads ANTHROPIC_API_KEY from environment, prompts user for input,
streams the response token-by-token, then prints usage stats.
-}
module Main (main) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Streaming
import Anthropic.Claude.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  apiKey <- case maybeKey of
    Nothing -> die "Error: ANTHROPIC_API_KEY environment variable not set"
    Just key -> pure $ ApiKey (T.pack key)

  env <- mkClientEnv apiKey

  putStr "Enter your prompt: "
  hFlush stdout
  prompt <- TIO.getLine

  if T.null prompt
    then die "Error: Prompt cannot be empty"
    else pure ()

  let request = mkRequest claude4Sonnet [userMsg prompt] 4096

  putStrLn ""
  finalMsg <- forEachEvent env request $ \evt ->
    case evt of
      ContentBlockDelta payload ->
        case contentBlockDeltaDelta payload of
          TextDelta txt -> do
            TIO.putStr txt
            hFlush stdout
          _ -> pure ()
      _ -> pure ()

  putStrLn "\n"
  let usage = responseUsage finalMsg
  putStrLn $ "Input tokens:  " ++ show (usageInputTokens usage)
  putStrLn $ "Output tokens: " ++ show (usageOutputTokens usage)
