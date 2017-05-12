{-# LANGUAGE OverloadedStrings #-}

module OpenMailbox (openMailbox) where

import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.ByteString.Lazy as ByteString

import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

import ListNameplates (listNameplates, expectAck)

openMailbox :: WebSockets.ClientApp ()
openMailbox conn = do
  nameplates <- listNameplates conn
  putStrLn $ show nameplates
  case nameplates of
    []     -> putStrLn "No nameplates, giving up."
    (nameplate:nameplates) -> do
      WebSockets.sendBinaryData conn $ Model.Claim nameplate
      expectAck conn

      msg <- WebSockets.receiveData conn
      case msg of
        Model.Claimed mailbox -> do
          WebSockets.sendBinaryData conn $ Model.Open mailbox
          expectAck conn

          msg <- WebSockets.receiveData conn
          case msg of
            Model.Letter side phase body server_rx server_tx messageID -> do
              TextIO.putStrLn side
              TextIO.putStrLn phase
              TextIO.putStrLn body
              TextIO.putStrLn messageID

            anything                 -> do
              putStr "Received unexpected message: "
              ByteString.putStr $ encode anything
              ByteString.putStr "\n"

        anything                 -> do
          putStr "Received unexpected message: "
          ByteString.putStr $ encode anything
          ByteString.putStr "\n"
