{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (putStr, concat)

import MagicWormholeModel (Message(..))

import Data.List (intersperse)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStr)
import Data.Text.Lazy.Encoding (decodeUtf8)


import Data.Aeson (Value(Null), encode, decode)

main = do
  putStr runTest
  putStr "\n"

runTest :: Text
runTest =
  let
    cases = [
      Welcome 123.456,
      Ack 456.789 "abc123",
      Bind "example.com" "client",
      List,
      Nameplates ["123-nameplate", "456-nameplate"],
      Allocate,
      Allocated "123-nameplate",
      Claim "123-nameplate",
      Claimed "abc-mailbox",
      Release "123-nameplate",
      Released,
      Open"abc-mailbox",
      Letter "client" "body" "message id",
      Close "happy" ,
      Closed,
      Error "message" Null,
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
