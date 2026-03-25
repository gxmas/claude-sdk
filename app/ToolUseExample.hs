{-# LANGUAGE OverloadedStrings #-}

-- |
-- Tool use example for the Claude SDK.
--
-- Demonstrates defining a tool, sending a request that triggers tool use,
-- extracting the tool call, and sending back a result.
module Main (main) where

import Anthropic.Claude (withLogger)
import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Messages
import Anthropic.Claude.Tools
import Anthropic.Claude.Types
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

  env <- withLogger debugLogger <$> mkClientEnv apiKey

  -- Define a weather tool using type-safe schema combinators
  let weatherTool =
        defineTool
          "get_weather"
          "Get the current weather"
          [ required
              "location"
              (withDescription "City and state" stringSchema)
          ]

  -- Send request with tool
  let req =
        (mkRequest claude4Sonnet [userMsg "What's the weather in San Francisco?"] 1024)
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
          mapM_
            ( \(tid, name, input) -> do
                putStrLn $ "  Tool: " ++ T.unpack name
                putStrLn $ "  Input: " ++ show input

                -- Simulate tool execution with different result types
                --
                -- TYPE-SAFE TOOL RESULT CONSTRUCTION:
                -- Three methods available, all enforce API compliance at compile time

                -- METHOD 1: buildToolResult - for JSON objects (encodes to text automatically)
                -- Use when you have structured data to return
                let resultBlock = buildToolResult tid $
                      object ["temperature" .= (72 :: Int), "unit" .= ("F" :: T.Text)]
                    -- Produces: ToolResultText "{\"temperature\":72,\"unit\":\"F\"}"

                -- METHOD 2: toolResultText - for simple text responses
                -- Use for plain text results
                -- let resultBlock = toolResultText tid "72°F and sunny" Nothing

                -- METHOD 3: toolResultBlocks - for rich multi-part content
                -- Use when you need multiple content blocks (text + images, formatted output, etc.)
                -- let resultBlock = toolResultBlocks tid
                --       [ textBlock "Current weather in San Francisco:"
                --       , textBlock "Temperature: 72°F, Condition: Sunny"
                --       ] Nothing

                -- ERROR HANDLING: If tool execution fails, use buildToolError
                -- let resultBlock = buildToolError tid "Location not found"
                -- Note: buildToolError now takes Text directly (type-safe!)

                -- Send result back
                let followUp =
                      ( mkRequest
                          claude4Sonnet
                          [ userMsg "What's the weather in San Francisco?",
                            assistantMsgBlocks (responseContent msg),
                            Message User (BlocksContent [resultBlock])
                          ]
                          1024
                      )
                        { requestTools = Just [weatherTool]
                        }

                putStrLn "\nSending tool result back..."
                finalResult <- createMessage env followUp
                case finalResult of
                  Left err2 -> putStrLn $ "Error: " ++ show err2
                  Right resp2 -> do
                    let finalText = extractText (apiResponseBody resp2)
                    TIO.putStrLn $ "\nFinal response: " <> finalText
            )
            calls
