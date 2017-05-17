module Main (main) where

import qualified System.Posix.Env.ByteString as Env (getArgs)
import qualified Data.Text.Encoding as Encoding (decodeUtf8)
import qualified Data.Text.Lazy as Text (fromStrict)

import Network.Socket (withSocketsDo)

import Receive (receiveApp)

import qualified Network.WebSockets as WebSockets

host :: String
host = "localhost"
-- host = "relay.magic-wormhole.io"

port :: Int
port = 4000

main :: IO ()
main = do
  args <- Env.getArgs
  let textArgs = map (Text.fromStrict . Encoding.decodeUtf8) args
  withSocketsDo $ WebSockets.runClient host port "/v1" $ receiveApp textArgs
