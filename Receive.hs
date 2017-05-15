{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat)

import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Aeson (encode)

import Network.WebSockets (Connection, ClientApp, receiveData)

import qualified MagicWormholeModel as Model
import ListNameplates (WormholeError(..))
import OpenMailbox (openMailbox)

receive :: Text -> Text -> Connection -> IO (Either WormholeError ())
receive appid side conn = do
  _ <- openMailbox appid side conn

  msg <- receiveData conn
  case msg of
    Model.Letter side phase body server_rx server_tx messageID -> do
      putStrLn $ concat [side, phase, body, messageID]
      pure $ Right ()

    anything                 -> do
      pure $ Left (UnexpectedMessage anything)


receiveApp :: ClientApp ()
receiveApp conn = do
  result <- receive "appid" "server" conn
  case result of
    Left (UnexpectedMessage anything) ->
      putStrLn $ concat ["Received unexpected message: ", decodeUtf8 $ encode anything, "\n"]
    Left anything -> putStrLn "some other error"
    Right anything ->
      putStrLn "Success"
  pure $ ()
