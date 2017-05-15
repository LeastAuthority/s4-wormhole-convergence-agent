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

import ListNameplates (WormholeError(..), listNameplates, expectAck)

claim :: Text -> WebSockets.ClientApp (Either WormholeError Text)
claim nameplate conn = do
      WebSockets.sendBinaryData conn $ Model.Claim nameplate
      expectAck conn

      msg <- WebSockets.receiveData conn
      case msg of
        Model.Claimed mailbox -> pure $ Right mailbox
        anything              -> pure $ Left (UnexpectedMessage msg)


open :: Text -> WebSockets.ClientApp (Either WormholeError ())
open mailbox conn = do
  WebSockets.sendBinaryData conn $ Model.Open mailbox
  ack <- expectAck conn
  case ack of
    Left anything -> pure $ Left anything
    Right anything -> pure $ Right ()


openMailbox :: Text -> Text -> WebSockets.ClientApp (Either WormholeError ())
openMailbox appid side conn = do
  nameplates <- listNameplates appid side conn

  case nameplates of
    Left anything                -> pure $ Left anything
    Right []                     -> pure $ Left NoNameplates
    Right (nameplate:nameplates) -> do
      putStrLn $ concat $ intersperse " " ("Nameplates: ":nameplate:nameplates)
      mailbox <- claim nameplate conn
      case mailbox of
        Left anything -> pure $ Left anything
        Right name    -> open name conn
