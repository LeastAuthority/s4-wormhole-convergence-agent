{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (putStr, concat)

import MagicWormholeModel (Message(Welcome, Ack, Bind, List))

import Data.List (intersperse)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStr)
import Data.Text.Lazy.Encoding (decodeUtf8)


import Data.Aeson (encode, decode)

main = do
  putStr runTest
  putStr "\n"

runTest :: Text
runTest =
  let
    cases = [
      Welcome 123.456,
      Ack 456.789 ("abc123" :: Text),
      Bind "example.com" "client",
      List
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
