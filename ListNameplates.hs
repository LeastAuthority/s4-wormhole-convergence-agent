{-# LANGUAGE OverloadedStrings #-}

module ListNameplates (listNameplates) where

import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.ByteString.Lazy as ByteString

import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

data State = Done | Starting | Bound

expectAck conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Ack server_tx id   -> putStrLn "Received ack..."
    Model.UndecodableMessage -> putStrLn "Received undecodable message!"
    anything                 -> do
      putStr "Received unexpected message: "
      ByteString.putStr $ encode anything
      ByteString.putStr "\n"


listNameplates :: WebSockets.ClientApp ()
listNameplates conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Welcome server_tx  -> putStrLn "Received welcome..."
    Model.UndecodableMessage -> putStrLn "Received undecodable message!"
    anything                 -> do
      putStr "Received unexpected message: "
      ByteString.putStr $ encode anything
      ByteString.putStr "\n"

  WebSockets.sendBinaryData conn $ Model.Bind "app" "server"
  expectAck conn

  WebSockets.sendBinaryData conn Model.List
  expectAck conn

  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Nameplates nameplates   -> do
      putStr "Nameplates: "
      TextIO.putStrLn $ Text.concat $ List.intersperse " " nameplates
    Model.UndecodableMessage -> putStrLn "Received undecodable message!"
    anything                 -> do
      putStr "Received unexpected message: "
      ByteString.putStr $ encode anything
      ByteString.putStr "\n"
