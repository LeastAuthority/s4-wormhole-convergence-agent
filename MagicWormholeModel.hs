{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeModel where

import GHC.Generics (Generic)

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.ByteString.Lazy (ByteString)

import Data.Aeson (
  Value(Object), ToJSON(..), FromJSON(..),
  object, encode, decode,
  (.=), (.:)
  )

import qualified Network.WebSockets as WebSockets

data Message =
  -- server-transmission-time
  Welcome Double |
  -- server-transmission-time, message-id
  Ack Double (Maybe Text) |
  -- appid, side
  Bind Text Text |
  List |
  -- nameplates
  Nameplates [Text] |
  Allocate |
  -- nameplate
  Allocated Text |
  -- nameplate
  Claim Text |
  -- mailbox
  Claimed Text |
  -- nameplate
  Release Text |
  Released |
  -- mailbox
  Open Text |
  -- side, phase, body, server_rx, server_tx, messageID
  Letter Text Text Text Double Double Text |
  -- phase, body
  Add Text Text |
  -- mood
  Close Text |
  Closed |
  -- XXX Something is wrong with the `original` field.  It doesn't seem to
  -- want to decode and I can't seem to figure out how to construct a test to
  -- exercise this case.
  -- message, original
  Error Text Value |
  -- ping
  Ping Int |
  -- ping
  Pong Int |
  -- This one is different.  We get this if decoding fails.
  -- raw
  UndecodableMessage ByteString
  deriving (Eq, Generic, Show)


instance FromJSON Message where
  parseJSON (Object v) = do
    messageType <- v .: "type"
    case (messageType :: Text) of
      "welcome"     -> Welcome <$> v .: "server_tx"
      "ack"         -> Ack     <$> v .: "server_tx" <*> v .: "id"
      "bind"        -> Bind    <$> v .: "appid" <*> v .: "side"
      "list"        -> pure List
      "nameplates"  -> Nameplates <$> (v .: "nameplates" >>= mapM (\np -> np .: "id"))
      "allocate"    -> pure Allocate
      "allocated"   -> Allocated <$> v .: "nameplate"
      "claim"       -> Claim   <$> v .: "nameplate"
      "claimed"     -> Claimed <$> v .: "mailbox"
      "release"     -> Release <$> v .: "nameplate"
      "released"    -> pure Released
      "open"        -> Open    <$> v .: "mailbox"
      "message"     ->
        Letter
        <$> v .: "side"
        <*> v .: "phase"
        <*> v .: "body"
        <*> v .: "server_rx"
        <*> v .: "server_tx"
        <*> v .: "id"
      "add"         -> Add     <$> v .: "phase" <*> v .: "body"
      "close"       -> Close   <$> v .: "mood"
      "closed"      -> pure Closed
      "error"       -> Error   <$> v .: "message" <*> v .: "orig"
      "ping"        -> Ping    <$> v .: "ping"
      "pong"        -> Pong    <$> v .: "pong"
      _             -> fail "unknown message type"
  parseJSON _ = fail "unknown message type"


nameplateToJSON :: Text -> Value
nameplateToJSON nameplate = object ["id" .= nameplate]

nameplatesToJSON :: [Text] -> [Value]
nameplatesToJSON = map nameplateToJSON

instance ToJSON Message where
  toJSON (Welcome server_tx) =
    object ["type" .= ("welcome" :: Text), "server_tx" .= server_tx]
  toJSON (Ack server_tx msgid) =
    object ["type" .= ("ack" :: Text), "server_tx" .= server_tx, "id" .= msgid]
  toJSON (Bind appid side) =
    object ["type" .= ("bind" :: Text), "appid" .= appid, "side" .= side]
  toJSON List =
    object ["type" .= ("list" :: Text)]
  toJSON (Nameplates nameplates) =
    object ["type" .= ("nameplates" :: Text), "nameplates" .= (nameplatesToJSON nameplates)]
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
  toJSON (Letter side phase body server_rx server_tx messageID) =
    object [
      "type" .= ("message" :: Text)
    , "phase" .= phase
    , "side" .= side
    , "body" .= body
    , "server_rx" .= server_rx
    , "server_tx" .= server_tx
    , "id" .= messageID
    ]
  toJSON (Add phase body) =
    object [
      "type" .= ("add" :: Text)
    , "phase" .= phase
    , "body" .= body
    ]
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
  toJSON (UndecodableMessage raw) =
    object ["type" .= ("undecodable" :: Text), "raw" .= (decodeUtf8 raw)]


decode' :: ByteString -> Message
decode' bytestring =
  case decode bytestring of
    Just msg -> msg
    Nothing -> UndecodableMessage bytestring

instance WebSockets.WebSocketsData Message where
  fromDataMessage (WebSockets.Binary b) = decode' b
  fromDataMessage (WebSockets.Text b _) = decode' b
  fromLazyByteString = decode'
  toLazyByteString = encode
