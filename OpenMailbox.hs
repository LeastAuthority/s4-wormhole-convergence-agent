{-# LANGUAGE OverloadedStrings #-}

module OpenMailbox (openMailbox) where

import Prelude hiding (concat, putStrLn)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStrLn)

import Data.List (intersperse)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

import LoggingWebSockets (receiveData, sendBinaryData)

import ListNameplates (WormholeError(..), listNameplates, expectAck)

claim :: Text -> WebSockets.ClientApp (Either WormholeError Text)
claim nameplate conn = do
      sendBinaryData conn $ Model.Claim nameplate
      _ <- expectAck conn

      msg <- receiveData conn
      case msg of
        Model.Claimed mailbox -> pure $ Right mailbox
        _                     -> pure $ Left (UnexpectedMessage msg)


open :: Text -> WebSockets.ClientApp (Either WormholeError ())
open mailbox conn = do
  sendBinaryData conn $ Model.Open mailbox
  ack <- expectAck conn
  case ack of
    Left anything -> pure $ Left anything
    Right _       -> pure $ Right ()


openMailbox :: Text -> Text -> WebSockets.ClientApp (Either WormholeError ())
openMailbox appid side conn = do
  nameplateNames <- listNameplates appid side conn

  case nameplateNames of
    Left anything                -> pure $ Left anything
    Right []                     -> pure $ Left NoNameplates
    Right (nameplate:nameplates) -> do
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplate:nameplates)
      mailbox <- claim nameplate conn
      case mailbox of
        Left anything -> pure $ Left anything
        Right name    -> open name conn
