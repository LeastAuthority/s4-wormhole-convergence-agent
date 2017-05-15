{-# LANGUAGE OverloadedStrings #-}

module ListNameplates (listNameplates, showNameplates, expectAck) where

import Prelude hiding (putStrLn, concat)

import Control.Monad.Except (MonadError, throwError, catchError)

import Data.List (intersperse)
import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.ByteString.Lazy.Char8 (unpack)

import Data.Text.Lazy.Encoding (decodeUtf8)

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
showNameplates conn =
  listNameplates conn >>=
  putStrLn . concat . intersperse " " . ("Nameplates: ":)


listNameplates :: WebSockets.ClientApp [Text]
listNameplates conn = do
  msg <- WebSockets.receiveData conn
  case (msg) of
    Model.Welcome server_tx  -> do
      putStrLn "Received welcome..."
      WebSockets.sendBinaryData conn $ Model.Bind "appid" "server"
      ack <- expectAck conn
      case (ack) of
        Right status -> putStrLn status
        Left err     -> putStrLn $ pack $ show err

      WebSockets.sendBinaryData conn Model.List
      ack <- expectAck conn
      case (ack) of
        Right status -> putStrLn status
        Left err     -> putStrLn $ pack $ show err

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
