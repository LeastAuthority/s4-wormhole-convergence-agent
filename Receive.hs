{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat, init)

import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteArray (ByteArrayAccess, convert)

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import Data.Hex (hex)

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)

import Crypto.KDF.HKDF (PRK, expand, extract)

import Crypto.Saltine.Class (decode)
import Crypto.Saltine.Core.SecretBox (Key, Nonce, secretbox, newNonce)

import Data.Aeson (encode)

import Network.WebSockets (Connection, ClientApp, receiveData, sendBinaryData)

import SPAKE2 (start, finish)

import qualified MagicWormholeModel as Model
import ListNameplates (WormholeError(..), expectAck)
import OpenMailbox (openMailbox)


sha256 :: ByteString -> ByteString
sha256 b = fromStrict $ convert $ (hash (toStrict b) :: Digest SHA256)


wormhole_purpose :: ByteString -> ByteString -> ByteString
wormhole_purpose side phase =
  B.concat ["wormhole:phase:" :: ByteString, sha256 side, sha256 phase]


prk :: ByteString -> PRK SHA256
prk skm =
  extract ("" :: S.ByteString) (toStrict skm)


hkdf :: ByteString -> Int -> ByteString -> ByteString
hkdf skm outlen ctxinfo =
  fromStrict (expand (prk skm) (toStrict ctxinfo) outlen)


derive_key :: ByteString -> ByteString -> ByteString
derive_key key purpose =
  hkdf key 32 purpose

derive_phase_key :: ByteString -> ByteString -> ByteString -> ByteString
derive_phase_key key side phase =
  derive_key key (wormhole_purpose side phase)

encrypt_data :: Key -> Nonce -> ByteString -> ByteString
encrypt_data key nonce plaintext =
  fromStrict $ secretbox key nonce (toStrict plaintext)

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
          in
            do
              let data_key = derive_phase_key (encodeUtf8 code) (finish body side) (encodeUtf8 phase)
              case decode (toStrict data_key) of
                Nothing -> pure $ Left CryptoFailure
                Just k  -> do
                  nonce <- newNonce
                  let ciphertext = encrypt_data k nonce (encodeUtf8 plaintext)
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
