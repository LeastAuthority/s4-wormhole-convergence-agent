{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeModel where

import GHC.Generics (Generic)

import Data.Text.Lazy (Text)

import Data.Aeson (
  Value(Object), ToJSON(..), FromJSON(..),
  genericToEncoding, defaultOptions, object, withObject,
  (.=), (.:)
  )


data Message =
  Welcome { server_tx :: Double } |
  Ack { server_tx :: Double, id :: Text } |
  Bind { appid :: Text, side :: Text } |
  List |
  Nameplates { nameplates :: [Text] } |
  Allocate |
  Allocated { nameplate :: Text } |
  Claim { nameplate :: Text } |
  Claimed { mailbox :: Text } |
  Release { nameplate :: Text } |
  Released |
  Open { mailbox :: Text } |
  Letter { side :: Text, body :: Text, messageID :: Text } |
  Close { mood :: Text } |
  Closed |
  Error { message :: Text, original :: Value } |
  Ping { ping :: Int } |
  Pong { pong :: Int } deriving (Eq, Generic, Show)


instance FromJSON Message where
  parseJSON (Object v) = do
    messageType <- v .: "type"
    case (messageType :: Text) of
      "welcome"     -> Welcome <$> v .: "server_tx"
      "ack"         -> Ack     <$> v .: "server_tx" <*> v .: "id"
      "bind"        -> Bind    <$> v .: "appid" <*> v .: "side"
      "list"        -> pure List
      "nameplates"  -> Nameplates <$> v .: "nameplates"
      "allocate"    -> pure Allocate
      "allocated"   -> Allocated <$> v .: "nameplate"
      "claim"       -> Claim   <$> v .: "nameplate"
      "claimed"     -> Claimed <$> v .: "mailbox"
      "release"     -> Release <$> v .: "nameplate"
      "released"    -> pure Released
      "open"        -> Open    <$> v .: "mailbox"
      "message"     -> Letter  <$> v .: "side" <*> v .: "body" <*> v .: "msg_id"
      "close"       -> Close   <$> v .: "mood"
      "closed"      -> pure Closed
      "error"       -> Error   <$> v .: "message" <*> v .: "orig"
      "ping"        -> Ping    <$> v .: "ping"
      "pong"        -> Pong    <$> v .: "pong"
      _             -> fail "unknown message type"


instance ToJSON Message where
  toJSON (Welcome server_tx) =
    object ["type" .= ("welcome" :: Text), "server_tx" .= server_tx]
  toJSON (Ack server_tx id) =
    object ["type" .= ("ack" :: Text), "server_tx" .= server_tx, "id" .= id]
  toJSON (Bind appid side) =
    object ["type" .= ("bind" :: Text), "appid" .= appid, "side" .= side]
  toJSON List =
    object ["type" .= ("list" :: Text)]
  toJSON (Nameplates nameplates) =
    object ["type" .= ("nameplates" :: Text), "nameplates" .= nameplates]
  toJSON Allocate =
    object ["type" .= ("allocate" :: Text)]
  toJSON (Allocated nameplate) =
    object ["type" .= ("allocated" :: Text), "nameplate" .= nameplate]
  toJSON (Claim nameplate) =
    object ["type" .= ("claim" :: Text), "nameplate" .= nameplate]
  toJSON (Claimed mailbox) =
    object ["type" .= ("claimed" :: Text), "mailbox" .= mailbox]
  toJSON (Release nameplate) =
    object ["type" .= ("release" :: Text), "nameplate" .= nameplate]
  toJSON Released =
    object ["type" .= ("released" :: Text)]
  toJSON (Open mailbox) =
    object ["type" .= ("open" :: Text), "mailbox" .= mailbox]
  toJSON (Letter side body messageID) =
    object ["type" .= ("message" :: Text), "side" .= side, "body" .= body, "msg_id" .= messageID]
  toJSON (Close mood) =
    object ["type" .= ("close" :: Text), "mood" .= mood]
  toJSON Closed =
    object ["type" .= ("closed" :: Text)]
  toJSON (Error message original) =
    object ["type" .= ("error" :: Text), "message" .= message, "orig" .= original]
  toJSON (Ping ping) =
    object ["type" .= ("ping" :: Text), "ping" .= ping]
  toJSON (Pong pong) =
    object ["type" .= ("pong" :: Text), "pong" .= pong]
