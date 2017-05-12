{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

import Data.Text.Lazy (Text, concat)
import qualified Data.Text.Lazy.IO as TextIO

import GHC.Generics (Generic)

import Data.Aeson (
  Value(Object), ToJSON(..), FromJSON(..),
  genericToEncoding, defaultOptions, object, withObject,
  encode, decode,
  (.=), (.:)
  )

data Message = Nameplates { nameplates :: [Text] } deriving (Eq, Generic, Show)

parseNameplates v =

instance FromJSON Message where
  parseJSON (Object v) = do
    messageType <- v .: "type"
    case (messageType :: Text) of
      "nameplates"  -> parseNameplates v

main =
  let
    x = decode "{\"type\": \"nameplates\", \"nameplates\": [{\"id\": \"1\"}]}"
  in
    case x of
      Nothing -> putStrLn "oops"
      Just (Nameplates n) -> TextIO.putStrLn $ Data.Text.Lazy.concat n
