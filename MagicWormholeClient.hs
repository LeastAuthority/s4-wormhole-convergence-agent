{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeClient where

import Prelude hiding (id)

import qualified Data.Text.Lazy as LazyT

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

import LoggingWebSockets (receiveData, sendBinaryData)

data WormholeError = UnexpectedMessage Model.Message
                   | CryptoFailure deriving (Show, Eq)


expectAck :: WebSockets.Connection -> IO (Either WormholeError ())
expectAck conn = do
  msg <- receiveData conn
  case (msg) of
    Model.Ack _ _ -> pure $ Right ()
    _             -> pure $ Left $ UnexpectedMessage msg


claim :: LazyT.Text -> WebSockets.ClientApp (Either WormholeError LazyT.Text)
claim nameplate conn = do
      sendBinaryData conn $ Model.Claim nameplate (Just "c")
      _ <- expectAck conn

      msg <- receiveData conn
      case msg of
        Model.Claimed mailbox -> pure $ Right mailbox
        _                     -> pure $ Left (UnexpectedMessage msg)


open :: LazyT.Text -> WebSockets.ClientApp (Either WormholeError ())
open mailbox conn = do
  sendBinaryData conn $ Model.Open mailbox (Just "d")
  ack <- expectAck conn
  case ack of
    Left anything -> pure $ Left anything
    Right _       -> pure $ Right ()


bind :: LazyT.Text -> LazyT.Text -> WebSockets.ClientApp (Either WormholeError ())
bind appid side conn = do
  sendBinaryData conn $ Model.Bind appid side (Just "a")
  expectAck conn


list :: WebSockets.ClientApp (Either WormholeError [LazyT.Text])
list conn = do
  sendBinaryData conn $ Model.List (Just "b")
  _ <- expectAck conn

  nameplatesMsg <- receiveData conn
  case (nameplatesMsg) of
    Model.Nameplates nameplates ->
      pure $ Right nameplates
    _                           ->
      pure $ Left (UnexpectedMessage nameplatesMsg)
