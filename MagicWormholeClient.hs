{-# LANGUAGE OverloadedStrings #-}

module MagicWormholeClient where

import Control.Monad.IO.Class (liftIO)

import qualified Text.JSON as JSON

import Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WebSockets

app :: WebSockets.ClientApp ()
app conn = do
  welcome <- WebSockets.receiveData conn
  liftIO $ T.putStrLn welcome

  WebSockets.sendTextData conn ("{\"type\": \"bind\", \"appid\": \"testing\", \"side\": \"me\"}" :: Text)
  ack <- WebSockets.receiveData conn
  liftIO $ T.putStrLn ack

  WebSockets.sendTextData conn ("{\"type\": \"list\"}" :: Text)
  nameplates <- WebSockets.receiveData conn
  liftIO $ T.putStrLn nameplates
