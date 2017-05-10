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
  Bind { appid :: String, side :: String } |
  List deriving (Eq, Generic, Show)


instance FromJSON Message where
  parseJSON (Object v) = do
    messageType <- v .: "type"
    case (messageType :: Text) of
      "welcome"     -> Welcome <$> v .: "server_tx"
      "ack"         -> Ack     <$> v .: "server_tx" <*> v .: "id"
      "bind"        -> Bind    <$> v .: "appid" <*> v .: "side"
      "list"        -> List
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
