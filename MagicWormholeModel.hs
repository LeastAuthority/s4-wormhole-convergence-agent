{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeModel where

import GHC.Generics (Generic)

import Data.Text (Text)

import Data.Aeson (
  Object, ToJSON(..), FromJSON(..),
  genericToEncoding, defaultOptions, object, withObject,
  (.=), (.:)
  )


data Message =
  Welcome { server_tx :: Double } |
  Ack { server_tx :: Double, id :: Text } |
  Bind { appid :: String, side :: String } |
  List deriving (Generic, Show)


instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Ack
    <$> v .: "id"
    <*> v .: "server_tx"


instance ToJSON Message where
  toJSON (Bind appid side) =
    object ["type" .= ("bind" :: Text), "appid" .= appid, "side" .= side]
  toJSON List =
    object ["type" .= ("list" :: Text)]
