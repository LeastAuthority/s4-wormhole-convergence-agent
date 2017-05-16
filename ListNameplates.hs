{-# LANGUAGE OverloadedStrings #-}

module ListNameplates (WormholeError(..), listNameplates, showNameplates, expectAck) where

import Prelude hiding (putStrLn, concat)

import Data.List (intersperse)

import Data.Text.Lazy (Text, concat, append, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Aeson (encode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

import LoggingWebSockets (receiveData, sendBinaryData)

data WormholeError = UnexpectedMessage Model.Message
                   | NoNameplates
                   | CryptoFailure deriving (Show, Eq)


expectAck :: WebSockets.Connection -> IO (Either WormholeError Text)
expectAck conn = do
  msg <- receiveData conn
  case (msg) of
    Model.Ack _ _ -> pure $ Right "Received ack..."
    _             -> pure $ Left $ UnexpectedMessage msg




showNameplates :: Text -> Text-> WebSockets.ClientApp ()
showNameplates appid side conn = do
  nameplates <- listNameplates appid side conn
  case nameplates of
    Left (UnexpectedMessage msg) ->
      putStrLn $ append "Unexpected wormhole message: " (decodeUtf8 $ encode msg)
    Left anything                ->
      putStrLn $ append "Failure: " (pack $ show anything)
    Right nameplateNames         ->
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplateNames)


listNameplates :: Text -> Text -> WebSockets.ClientApp (Either WormholeError [Text])
listNameplates appid side conn = do
  welcomeMsg <- receiveData conn
  case (welcomeMsg) of
    Model.Welcome _  -> do
      sendBinaryData conn $ Model.Bind appid side (Just "a")
      _ <- expectAck conn

      sendBinaryData conn $ Model.List (Just "b")
      _ <- expectAck conn

      nameplatesMsg <- receiveData conn
      case (nameplatesMsg) of
        Model.Nameplates nameplates ->
          pure $ Right nameplates
        _                           ->
          pure $ Left (UnexpectedMessage nameplatesMsg)

    _ -> pure $ Left (UnexpectedMessage welcomeMsg)
