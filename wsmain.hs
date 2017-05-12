module Main (main) where

import Network.Socket (withSocketsDo)

import ListNameplates (showNameplates)
import OpenMailbox (openMailbox)

import qualified Network.WebSockets as WebSockets

host :: String
host = "localhost"
-- host = "relay.magic-wormhole.io"

port :: Int
port = 4000

-- main :: IO ()
-- main = withSocketsDo $ WebSockets.runClient host port "/v1" showNameplates

main :: IO ()
main = withSocketsDo $ WebSockets.runClient host port "/v1" openMailbox
