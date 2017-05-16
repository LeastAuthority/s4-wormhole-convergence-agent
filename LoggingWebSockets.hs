module LoggingWebSockets (receiveData, sendBinaryData) where

import qualified Network.WebSockets as WS (Connection, WebSocketsData, receiveData, sendBinaryData)

-- Get the ToJSON Model.Message instance
import MagicWormholeModel ()

receiveData :: (Show a, WS.WebSocketsData a) => WS.Connection -> IO a
receiveData conn = do
  msg <- WS.receiveData conn
  putStr ">> "
  putStrLn $ show msg
  pure msg

sendBinaryData :: (Show a, WS.WebSocketsData a) => WS.Connection -> a -> IO ()
sendBinaryData conn msg = do
  putStr "<< "
  putStrLn $ show msg
  WS.sendBinaryData conn msg
