{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeClient where

import Prelude hiding (id)

import Control.Monad.IO.Class (liftIO)

import Data.Aeson (encode, decode)

import Data.ByteString.Lazy (pack)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

sendMessage :: Model.Message -> WebSockets.ClientApp ()
sendMessage message conn = do
    WebSockets.sendTextData conn $ encode message
    response <- WebSockets.receiveData conn
    let
      ack = maybe (fail "oh no!") $ return $ (decode response)
    liftIO $ T.putStrLn (ack :: Model.Message)
    return ()


app :: WebSockets.ClientApp ()
app conn = do
  welcome <- WebSockets.receiveData conn
  liftIO $ T.putStrLn welcome

  sendMessage (Model.Bind "foo" "bar") conn
  sendMessage Model.List conn
  nameplates <- WebSockets.receiveData conn
  liftIO $ T.putStrLn nameplates
