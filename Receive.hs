{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat, init)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteArray (convert)

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as Base16 (encode)

import Data.Hex (hex)

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)

import Crypto.KDF.HKDF (PRK, expand, extract)

import Crypto.Saltine.Class (decode)
import Crypto.Saltine.Core.SecretBox (Key, Nonce, secretbox, newNonce)

import qualified Data.Aeson as Aeson (encode)

import Network.WebSockets (Connection, ClientApp)

import SPAKE2 (finish)

import LoggingWebSockets (receiveData, sendBinaryData)

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

  first_msg <- receiveData conn
  case first_msg of
    Model.Letter _ _ _ _ _ _ -> do
      sendBinaryData conn $ Model.Add "pake" $ decodeUtf8 $ Aeson.encode $ Model.PAKE appid code
      _ <- expectAck conn
      key <- getVersion side code conn
      case key of
        Left anything      ->
          pure $ Left anything

        Right keyBytes     -> do
          putStrLn $ decodeUtf8 $ fromStrict $ Base16.encode $ toStrict keyBytes
          pure $ Right ()

    anything                 -> do
      pure $ Left (UnexpectedMessage anything)


getVersion :: Text -> Text -> Connection -> IO (Either WormholeError ByteString)
getVersion mySide code conn = do
  msg <- receiveData conn
  case msg of
    Model.Letter theirSide _ body _ _ _ ->
      if theirSide == mySide then
        getVersion mySide code conn
      else
        let
          plaintext = ("{}" :: Text)
          phase = encodeUtf8 ("version" :: Text)
          key = finish body theirSide
          data_key = derive_phase_key (encodeUtf8 code) key phase
        in
          case decode (toStrict data_key) of
            Nothing -> pure $ Left CryptoFailure
            Just k  -> do
              nonce <- newNonce
              let ciphertext = encrypt_data k nonce (encodeUtf8 plaintext)
              _ <- sendBinaryData conn $ Model.Add "version" (decodeUtf8 $ hex ciphertext)
              _ <- expectAck conn

              pure $ Right key

    anything -> pure $ Left (UnexpectedMessage anything)



receiveApp :: ClientApp ()
receiveApp conn = do
  result <- receive "appid" "server" "code" conn
  case result of
    Left (UnexpectedMessage anything) ->
      putStrLn $ concat ["Received unexpected message: ", decodeUtf8 $ Aeson.encode anything, "\n"]
    Left _                            ->
      putStrLn "some other error"
    Right _                           ->
      putStrLn "Success"
  pure $ ()
