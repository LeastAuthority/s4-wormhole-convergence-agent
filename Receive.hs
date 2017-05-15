{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat)

import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.ByteString.Lazy (ByteString)

import Crypto.Hash (SHA256)
import Crypto.HKDF (hkdfExpand)

import Data.Aeson (encode)

import Network.WebSockets (Connection, ClientApp, receiveData)

import SPAKE2 (start, finish)

import qualified MagicWormholeModel as Model
import ListNameplates (WormholeError(..))
import OpenMailbox (openMailbox)

sha256 :: Text -> ByteString
sha256 plaintext =
  digest
  where
    digest = SHA256.finalize ctx
    ctx = SHA256.update ctx0 $ encodeUtf8 plaintext
    ctx0 = SHA256.init

wormhole_purpose :: Text -> Text -> ByteString
wormhole_purpose side phase =
  concat ["wormhole:phase:", sha256 side, sha256 phase]

hkdf :: ByteString -> Int -> ByteString -> ByteString
hkdf skm outlen ctxinfo =
  hkpfExpand SHA256 skm ctxinfo outlen

derive_key :: ByteString -> ByteString -> ByteString
derive_key key purpose =
  hkdf key 32 purpose

derive_phase_key :: Text -> Text -> Text
derive_phase_key key side phase =
  derive_key key $ wormhole_purpose side phase

receive :: Text -> Text -> Text -> Connection -> IO (Either WormholeError ())
receive appid side code conn = do
  _ <- openMailbox appid side conn

  msg <- receiveData conn
  case msg of
    Model.Letter side phase body server_rx server_tx messageID -> do
      sendBinaryData conn $ Model.Add "bluhphase" $ start appid code
      expectAck
      msg <- receiveData conn
      case msg of
        Model.Letter side phase body server_rx server_tx messageID ->
          let
            data_key = derive_phase_key $ finish body side $ "version"
            ciphertext = encrypt_data data_key plaintext
          in
            sendBinaryData conn $ Model.Add "version" ciphertext

    anything                 -> do
      pure $ Left (UnexpectedMessage anything)


receiveApp :: ClientApp ()
receiveApp conn = do
  result <- receive "appid" "server" conn
  case result of
    Left (UnexpectedMessage anything) ->
      putStrLn $ concat ["Received unexpected message: ", decodeUtf8 $ encode anything, "\n"]
    Left anything -> putStrLn "some other error"
    Right anything ->
      putStrLn "Success"
  pure $ ()
