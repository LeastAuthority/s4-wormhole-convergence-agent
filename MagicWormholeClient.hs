{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module MagicWormholeClient where

import Prelude hiding (id)

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (encode, decode)

import Data.ByteString.Lazy (pack)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

-- sendMessage :: Model.Message -> WebSockets.ClientApp ()
-- sendMessage message conn = do
--   WebSockets.sendTextData conn $ encode message
--   response <- WebSockets.receiveData conn
--   let
--     ack = maybe (fail "oh no!") return (decode response)
--   liftIO $ T.putStrLn (ack :: Model.Message)
--   return ()


-- app :: WebSockets.ClientApp ()
-- app conn = do
--   welcome <- WebSockets.receiveData conn
--   liftIO $ T.putStrLn welcome

--   sendMessage (Model.Bind "foo" "bar") conn
--   sendMessage Model.List conn
--   nameplates <- WebSockets.receiveData conn
--   liftIO $ T.putStrLn nameplates

send conn [] = conn
send conn (m:ms) =
  WebSockets.sendTextData conn $ encode m >>= (\conn -> send conn ms)


execute :: (State, [Model.Message]) -> WebSockets.ClientApp ()
execute (state, messages) conn = do
  send conn messages
  encodedMessage <- WebSockets.receiveData conn
  case (decode encodedMessage) :: Maybe Model.Message of
    Nothing      -> do
      print "decoding message failed"
      print encodedMessage
    Just message -> execute $ processMessage state message $ conn
