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

data WormholeError = UnexpectedMessage Model.Message
                   | NoNameplates deriving (Show, Eq)


expectAck :: WebSockets.Connection -> IO (Either WormholeError Text)
expectAck conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Ack server_tx id -> pure $ Right "Received ack..."
    anything               -> pure $ Left $ UnexpectedMessage msg




showNameplates :: Text -> Text-> WebSockets.ClientApp ()
showNameplates appid side conn = do
  nameplates <- listNameplates appid side conn
  case nameplates of
    Left (UnexpectedMessage msg) ->
      putStrLn $ append "Unexpected wormhole message: " (decodeUtf8 $ encode msg)
    Right nameplates             ->
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplates)


listNameplates :: Text -> Text -> WebSockets.ClientApp (Either WormholeError [Text])
listNameplates appid side conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Welcome server_tx  -> do
      WebSockets.sendBinaryData conn $ Model.Bind appid side
      expectAck conn

      WebSockets.sendBinaryData conn Model.List
      expectAck conn

      msg <- WebSockets.receiveData conn
      case (msg) of
        Model.Nameplates nameplates -> pure $ Right nameplates
        anything                    -> pure $ Left (UnexpectedMessage msg)

    anything -> pure $ Left (UnexpectedMessage msg)
