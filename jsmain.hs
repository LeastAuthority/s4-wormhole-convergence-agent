module Main (main) where

import MagicWormholeModel (Message(Welcome))

import qualified Data.ByteString.Lazy as B

import Data.Aeson (encode, decode)

main = do
  B.putStr $ encode $ Welcome 123.456
