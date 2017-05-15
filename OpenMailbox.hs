{-# LANGUAGE OverloadedStrings #-}

module OpenMailbox (openMailbox) where

import Prelude hiding (concat, putStrLn)

import Data.Text.Lazy (Text, pack, concat)
import Data.Text.Lazy.IO (putStrLn)
import qualified Data.ByteString.Lazy as ByteString

import Data.List (intersperse)

import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

import ListNameplates (listNameplates, expectAck)

openMailbox :: WebSockets.ClientApp ()
openMailbox conn = do
  nameplates <- listNameplates conn

  case nameplates of
    []     -> putStrLn "No nameplates, giving up."
    (nameplate:nameplates) -> do
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplate:nameplates)
      WebSockets.sendBinaryData conn $ Model.Claim nameplate
      ack <- expectAck conn
      case (ack) of
        Right status -> putStrLn status
        Left err     -> putStrLn $ pack $ show err

      msg <- WebSockets.receiveData conn
      case msg of
        Model.Claimed mailbox -> do
          WebSockets.sendBinaryData conn $ Model.Open mailbox
          ack <- expectAck conn
          case (ack) of
            Right status -> putStrLn status
            Left err     -> putStrLn $ pack $ show err

          msg <- WebSockets.receiveData conn
          case msg of
            Model.Letter side phase body server_rx server_tx messageID -> do
              putStrLn side
              putStrLn phase
              putStrLn body
              putStrLn messageID

            anything                 -> do
              putStr "Received unexpected message: "
              ByteString.putStr $ encode anything
              ByteString.putStr "\n"

        anything                 -> do
          putStr "Received unexpected message: "
          ByteString.putStr $ encode anything
          ByteString.putStr "\n"
