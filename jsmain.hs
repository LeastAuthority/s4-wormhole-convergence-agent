{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (putStr, concat)

import MagicWormholeModel (Message(..))

import Data.List (intersperse)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStr)
import Data.Text.Lazy.Encoding (decodeUtf8)


import Data.Aeson (Value(Null, Number), encode, decode)

main :: IO ()
main = do
  putStr runTest
  putStr "\n"

runTest :: Text
runTest =
  let
    cases = [
      Welcome 123.456,
      Ack 456.789 $ Just "abc123",
      Ack 123.456 Nothing,
      Bind "example.com" "client" Nothing,
      Bind "example.com" "client" (Just "message id"),
      List Nothing,
      List (Just "message id"),
      Nameplates ["123-nameplate", "456-nameplate"],
      Allocate Nothing,
      Allocate (Just "message id"),
      Allocated "123-nameplate",
      Claim "123-nameplate" Nothing,
      Claim "123-nameplate" (Just "message id"),
      Claimed "abc-mailbox",
      Release "123-nameplate" Nothing,
      Release "123-nameplate" (Just "message id"),
      Released,
      Open "abc-mailbox" Nothing,
      Open "abc-mailbox" (Just "message id"),
      Letter "client" "pake" "body" 123.456 456.789 (Just "message id"),
      Letter "client" "pake" "body" 123.456 456.789 Nothing,
      Add "version" "body" Nothing,
      Add "version" "body" (Just "message id"),
      Close "happy" Nothing,
      Close "happy" (Just "message id"),
      Closed,
      Error "message" Null,
      Error "message" $ Number 10,
      Ping 123,
      Pong 456
      ]
    results = map roundtrip cases
    newlines = intersperse "\n" results
  in
    concat newlines

roundtrip :: Message -> Text
roundtrip message =
  let
    encoded = encode message
    decoded = decode encoded :: Maybe Message
  in
    case decoded of
      Nothing       -> "decoding failed"
      Just actual   -> if actual == message then (decodeUtf8 encoded) else "not equal"
