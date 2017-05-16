{-# LANGUAGE OverloadedStrings #-}

module Receive (receiveApp) where

import Prelude hiding (putStrLn, concat, init)

import Control.Concurrent (threadDelay)

import Data.Text.Lazy (Text, concat)
import Data.Text.Lazy.IO (putStrLn)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteArray (convert)

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import Data.Hex (hex, unhex)

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)

import Crypto.KDF.HKDF (PRK, expand, extract)

import Crypto.Saltine.Class (decode)
import Crypto.Saltine.Internal.ByteSizes (secretBoxNonce)
import Crypto.Saltine.Core.SecretBox (Key, Nonce, newNonce, secretbox, secretboxOpen)

import qualified Data.Aeson as Aeson (encode)

import Network.WebSockets (Connection, ClientApp)

import SPAKE2 (finish)

import LoggingWebSockets (receiveData, sendBinaryData)

import qualified MagicWormholeModel as Model
import qualified MagicWormholeClient as Client

sha256 :: ByteString -> ByteString
sha256 b = fromStrict $ convert $ (hash (toStrict b) :: Digest SHA256)


wormhole_purpose :: Text -> Text -> ByteString
wormhole_purpose side phase =
  B.concat [
    "wormhole:phase:" :: ByteString
  , sha256 (encodeUtf8 side)
  , sha256 (encodeUtf8 phase)
  ]


prk :: ByteString -> PRK SHA256
prk skm =
  extract ("" :: S.ByteString) (toStrict skm)

hkdf :: ByteString -> Int -> ByteString -> ByteString
hkdf skm outlen ctxinfo =
  fromStrict (expand (prk skm) (toStrict ctxinfo) outlen)

derive_key :: ByteString -> ByteString -> ByteString
derive_key key purpose =
  hkdf key 32 purpose

derive_phase_key :: ByteString -> Text -> Text -> ByteString
derive_phase_key key side phase =
  derive_key key $ wormhole_purpose side phase

encrypt_data :: Key -> Nonce -> ByteString -> ByteString
encrypt_data key nonce plaintext =
  fromStrict $ secretbox key nonce (toStrict plaintext)

decrypt_data :: Key -> ByteString -> Maybe ByteString
decrypt_data key ciphertext =
  case (decode $ S.take (fromIntegral secretBoxNonce) (toStrict ciphertext)) of
    Nothing     ->
      Nothing
    Just nonce  ->
      fmap fromStrict $ secretboxOpen
      key
      nonce (S.drop (fromIntegral secretBoxNonce) (toStrict ciphertext))

hexToByteString :: Text -> Maybe ByteString
hexToByteString = unhex . encodeUtf8

byteStringToHex :: ByteString -> Text
byteStringToHex = decodeUtf8 . hex

receive :: Text -> Text -> Text -> Connection -> IO (Either Client.WormholeError Text)
receive appid side code conn = do
  welcome <- receiveData conn
  case welcome of
    Model.Welcome _ -> do
      _ <- Client.bind appid side conn
      _ <- openMailbox appid side code conn
      sessionKey <- negotiatePake appid side code conn
      case sessionKey of
        Left anything -> pure $ Left anything
        Right key     -> receivePayload appid side key conn

    _ ->
      pure $ Left Client.CryptoFailure

receivePayload :: Text -> Text -> ByteString -> Connection -> IO (Either Client.WormholeError Text)
receivePayload appid mySide sessionKey conn = do
  msg <- receiveData conn
  case msg of
    Model.Ack _ _ ->
      receivePayload appid mySide sessionKey conn

    Model.Letter theirSide theirPhase hextext _ _ _ ->
      if theirSide == mySide
      then receivePayload appid mySide sessionKey conn
      else
        case hexToByteString hextext of
          Nothing         ->
            pure $ Left Client.CryptoFailure
          Just ciphertext ->
            case theirPhase of
              "version"   ->
                -- Eh whatever I don't care.
                receivePayload appid mySide sessionKey conn

              anyPhase ->
                let
                  data_key_bytes = derive_phase_key sessionKey theirSide anyPhase
                  data_key = decode $ toStrict data_key_bytes
                in
                  case data_key of
                    Nothing  ->
                      pure $ Left Client.CryptoFailure
                    Just key ->
                      case decrypt_data key ciphertext of
                        Nothing         ->
                          pure $ Left Client.CryptoFailure
                        Just plaintext  -> do
                          pure $ Right $ decodeUtf8 plaintext
    _ ->
      -- not sure what this message is, time to fail.
      pure $ Left Client.CryptoFailure -- xxx not really a crypto failure wooops!


openMailbox :: Text -> Text -> Text -> Connection -> IO (Either Client.WormholeError ())
openMailbox appid side code conn = do
  nameplates <- Client.list conn
  case nameplates of
    Left anything ->
      -- Broken I guess.
      pure $ Left anything

    Right [] -> do
      -- Come back in a bit
      _ <- threadDelay 5000000
      openMailbox appid side code conn

    Right (nameplate:_) -> do
      mailbox <- Client.claim nameplate conn
      case mailbox of
        Left anything -> pure $ Left anything
        Right name -> Client.open name conn


negotiatePake :: Text -> Text -> Text -> Connection -> IO (Either Client.WormholeError ByteString)
negotiatePake appid side code conn = do
  _ <- sendOurPake appid side code conn
  processPake appid side code conn


sendOurPake :: Text -> Text -> Text -> Connection -> IO (Either Client.WormholeError ())
sendOurPake appid side code conn =
  let
    pake = decodeUtf8 $ Aeson.encode $ Model.PAKE appid code
    addpake = Model.Add "pake" pake (Just "e")
    key = decode $ toStrict $ derive_phase_key (encodeUtf8 code) side "version"
  in
    case key of
      Nothing ->
        pure $ Left Client.CryptoFailure
      Just k  -> do
        nonce <- newNonce
        let version = encrypt_data k nonce "{}"
        let addversion = Model.Add "version" (byteStringToHex version) (Just "f")

        _ <- sendBinaryData conn addpake
        _ <- sendBinaryData conn addversion
        pure $ Right ()


processPake :: Text -> Text -> Text -> Connection -> IO (Either Client.WormholeError ByteString)
processPake appid mySide code conn = do
  msg <- receiveData conn
  case msg of
    Model.Ack _ _ ->
      processPake appid mySide code conn

    Model.Letter theirSide theirPhase hextext _ _ _ ->
      if theirSide == mySide
      then processPake appid mySide code conn
      else
        case hexToByteString hextext of
          Nothing         ->
            pure $ Left Client.CryptoFailure
          Just ciphertext ->
            case theirPhase of
              "pake"   ->
                pure $ Right $ finish ciphertext theirSide
              _        ->
                pure $ Left Client.CryptoFailure
    _ ->
      -- not sure what this message is, time to fail.
      pure $ Left Client.CryptoFailure -- xxx not really a crypto failure wooops!



receiveApp :: ClientApp ()
receiveApp conn = do
  result <- receive "appid" "server" "code" conn
  case result of
    Left (Client.UnexpectedMessage anything) ->
      putStrLn $ concat ["Received unexpected message: ", decodeUtf8 $ Aeson.encode anything, "\n"]
    Left _                            ->
      putStrLn "some other error"
    Right msg                         ->
      putStrLn $ concat ["Success: ", msg]
  pure $ ()
