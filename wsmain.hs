module Main (main) where

import Network.Socket (withSocketsDo)

import MagicWormholeClient (app)

import qualified Network.WebSockets as WebSockets

host :: String
host = "localhost"
-- host = "relay.magic-wormhole.io"

port :: Int
port = 4000

main :: IO ()
main = withSocketsDo $ WebSockets.runClient host port "/v1" app
