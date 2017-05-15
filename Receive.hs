{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat, init)

import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteString.Lazy (ByteString, toStrict)

import Data.Hex (hex)

import Crypto.Hash.SHA256 (init, update, finalize)
import Crypto.HKDF (hkdfExpand)

import Crypto.Saltine.Class (decode)
import Crypto.Saltine.Core.SecretBox (Key, Nonce, secretbox, newNonce)

import Data.Aeson (encode)

import Network.WebSockets (Connection, ClientApp, receiveData, sendBinaryData)

import SPAKE2 (start, finish)

import qualified MagicWormholeModel as Model
import ListNameplates (WormholeError(..), expectAck)
import OpenMailbox (openMailbox)

sha256 :: Text -> ByteString
sha256 plaintext =
  digest
  where
    digest = finalize ctx
    ctx = update ctx0 $ encodeUtf8 plaintext
    ctx0 = init

wormhole_purpose :: Text -> Text -> ByteString
wormhole_purpose side phase =
  concat ["wormhole:phase:", sha256 side, sha256 phase]

hkdf :: ByteString -> Int -> ByteString -> ByteString
hkdf skm outlen ctxinfo =
  hkpfExpand SHA256 skm ctxinfo outlen

derive_key :: ByteString -> ByteString -> ByteString
derive_key key purpose =
  hkdf key 32 purpose

derive_phase_key :: ByteString -> ByteString -> ByteString
derive_phase_key key side phase =
  derive_key key $ wormhole_purpose side phase

encrypt_data :: Key -> ByteString -> IO ByteString
encrypt_data key plaintext = do
  nonce <- newNonce
  secretbox key nonce (toStrict plaintext)

receive :: Text -> Text -> Text -> Connection -> IO (Either WormholeError ())
receive appid side code conn = do
  _ <- openMailbox appid side conn

  msg <- receiveData conn
  case msg of
    Model.Letter side phase body server_rx server_tx messageID -> do
      sendBinaryData conn $ Model.Add "bluhphase" $ (decodeUtf8 $ hex $ start appid code)
      _ <- expectAck conn
      msg <- receiveData conn
      case msg of
        Model.Letter side phase body server_rx server_tx messageID ->
          let
            plaintext = ("{}" :: Text)
            phase = ("version" :: Text)
            data_key = derive_phase_key (finish body side) (encodeUtf8 phase)
          in
            do
              key_obj <- decode (toStrict data_key)
              case key_obj of
                Nothing -> pure $ Left CryptoFailure
                Just k  -> do
                  ciphertext <- encrypt_data k (encodeUtf8 plaintext)
                  _ <- sendBinaryData conn $ Model.Add "version" (decodeUtf8 $ hex ciphertext)
                  pure $ Right ()

    anything                 -> do
      pure $ Left (UnexpectedMessage anything)


receiveApp :: ClientApp ()
receiveApp conn = do
  result <- receive "appid" "server" "code" conn
  case result of
    Left (UnexpectedMessage anything) ->
      putStrLn $ concat ["Received unexpected message: ", decodeUtf8 $ encode anything, "\n"]
    Left anything -> putStrLn "some other error"
    Right anything ->
      putStrLn "Success"
  pure $ ()
