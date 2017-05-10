{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeClient where

import GHC.Generics

import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Lazy (pack)
import Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WebSockets

import Data.Aeson (
  ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions, object, decode, encode, (.=)
  )


data Message =
  Welcome |
  Bind { appid :: String, side :: String } |
  List deriving (Generic, Show)


instance ToJSON Message where
  toJSON (Bind appid side) =
    object ["type" .= ("bind" :: Text), "appid" .= appid, "side" .= side]
  toJSON List =
    object ["type" .= ("list" :: Text)]



sendMessage :: Message -> WebSockets.ClientApp ()
sendMessage message conn = do
    WebSockets.sendTextData conn $ encode message
    response <- WebSockets.receiveData conn
    let
      ack = decode $ pack response
    liftIO $ T.putStrLn ack
    return ()


app :: WebSockets.ClientApp ()
app conn = do
  welcome <- WebSockets.receiveData conn
  liftIO $ T.putStrLn welcome

  sendMessage (Bind "foo" "bar") conn
  sendMessage List conn
  -- nameplates <- WebSockets.receiveData conn
  -- liftIO $ T.putStrLn nameplates
