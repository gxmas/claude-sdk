{-# LANGUAGE OverloadedStrings #-}

{- |
Tool use example for the Claude SDK.

Demonstrates defining a tool, sending a request that triggers tool use,
extracting the tool call, and sending back a result.
-}
module Main (main) where

import Anthropic.Claude.Types
import Anthropic.Claude.Messages
import Anthropic.Claude.Tools
import Anthropic.Claude.Internal.HTTP
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (die)

main :: IO ()
main = do
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  apiKey <- case maybeKey of
    Nothing -> die "Error: ANTHROPIC_API_KEY environment variable not set"
    Just key -> pure $ ApiKey (T.pack key)

  env <- mkClientEnv apiKey

  -- Define a weather tool
  let weatherSchema = object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "location" .= object
                [ "type" .= ("string" :: T.Text)
                , "description" .= ("City and state" :: T.Text)
                ]
            ]
        , "required" .= (["location"] :: [T.Text])
        ]
      weatherTool = defineTool "get_weather" "Get the current weather" weatherSchema

  -- Send request with tool
  let req = (mkRequest claude4Sonnet [userMsg "What's the weather in San Francisco?"] 1024)
        { requestTools = Just [weatherTool]
        }

  putStrLn "Sending request with tool..."
  result <- createMessage env req

  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err

    Right resp -> do
      let msg = apiResponseBody resp
          calls = extractToolCalls msg

      if null calls
        then putStrLn $ "No tool calls. Response: " ++ T.unpack (extractText msg)
        else do
          putStrLn $ "Got " ++ show (length calls) ++ " tool call(s):"
          mapM_ (\(tid, name, input) -> do
            putStrLn $ "  Tool: " ++ T.unpack name
            putStrLn $ "  Input: " ++ show input

            -- Simulate tool execution
            let toolResult = object ["temperature" .= (72 :: Int), "unit" .= ("F" :: T.Text)]
                resultBlock = buildToolResult tid toolResult

            -- Send result back
            let followUp = (mkRequest claude4Sonnet
                  [ userMsg "What's the weather in San Francisco?"
                  , assistantMsgBlocks (responseContent msg)
                  , Message User (BlocksContent [resultBlock])
                  ] 1024)
                  { requestTools = Just [weatherTool] }

            putStrLn "\nSending tool result back..."
            finalResult <- createMessage env followUp
            case finalResult of
              Left err2 -> putStrLn $ "Error: " ++ show err2
              Right resp2 -> do
                let finalText = extractText (apiResponseBody resp2)
                TIO.putStrLn $ "\nFinal response: " <> finalText
            ) calls
