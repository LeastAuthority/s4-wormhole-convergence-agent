{-# LANGUAGE OverloadedStrings #-}

module ListNameplates (listNameplates, showNameplates, expectAck) where

import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

data State = Done | Starting | Bound

expectAck conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Ack server_tx id       -> putStrLn "Received ack..."
    Model.UndecodableMessage raw -> putStrLn ("Received undecodable message:" ++ (unpack raw))
    anything                 -> do
      putStr "Received unexpected message: "
      ByteString.putStr $ encode anything
      ByteString.putStr "\n"


showNameplates :: WebSockets.ClientApp ()
showNameplates conn =
  listNameplates conn >>=
  TextIO.putStrLn . Text.concat . List.intersperse " " . ("Nameplates: ":)


listNameplates :: WebSockets.ClientApp [Text.Text]
listNameplates conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Welcome server_tx  -> do
      putStrLn "Received welcome..."
      WebSockets.sendBinaryData conn $ Model.Bind "appid" "server"
      expectAck conn

      WebSockets.sendBinaryData conn Model.List
      expectAck conn

      msg <- WebSockets.receiveData conn
      case (msg) of
        Model.Nameplates nameplates  ->
          pure nameplates

        Model.UndecodableMessage raw ->
          pure ["Received undecodable message!", decodeUtf8 raw]

        anything                    ->
          pure ["Received unexpected message: ", decodeUtf8 $ encode anything, "\n"]

    Model.UndecodableMessage raw ->
      pure ["Received undecodable message!", decodeUtf8 raw]

    anything                     ->
      pure ["Received unexpected message: ", decodeUtf8 $ encode anything, "\n"]
