{-# LANGUAGE OverloadedStrings #-}

module ListNameplates (WormholeError(..), listNameplates, showNameplates, expectAck) where

import Prelude hiding (putStrLn, concat)

import Data.List (intersperse)

import Data.Text.Lazy (Text, concat, pack, append)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.ByteString.Lazy.Char8 (unpack)


import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

data State = Done | Starting | Bound

data WormholeError = UnexpectedMessage Model.Message deriving (Show, Eq)


expectAck :: WebSockets.Connection -> IO (Either WormholeError Text)
expectAck conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Ack server_tx id -> pure $ Right "Received ack..."
    anything               -> pure $ Left $ UnexpectedMessage msg




showNameplates :: WebSockets.ClientApp ()
showNameplates conn = do
  nameplates <- listNameplates conn
  case nameplates of
    Left (UnexpectedMessage msg) ->
      putStrLn $ append "Unexpected wormhole message: " (decodeUtf8 $ encode msg)
    Right nameplates             ->
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplates)


listNameplates :: WebSockets.ClientApp (Either WormholeError [Text])
listNameplates conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Welcome server_tx  -> do
      WebSockets.sendBinaryData conn $ Model.Bind "appid" "server"
      expectAck conn

      WebSockets.sendBinaryData conn Model.List
      expectAck conn

      msg <- WebSockets.receiveData conn
      case (msg) of
        Model.Nameplates nameplates -> pure $ Right nameplates
        anything                    -> pure $ Left (UnexpectedMessage msg)

    anything -> pure $ Left (UnexpectedMessage msg)
