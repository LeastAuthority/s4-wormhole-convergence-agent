module Main (main) where

import Network.Socket (withSocketsDo)

import MagicWormholeClient (app)

import qualified Network.WebSockets as WebSockets

main :: IO ()
main = withSocketsDo $ WebSockets.runClient "relay.magic-wormhole.io" 4000 "/v1" app
