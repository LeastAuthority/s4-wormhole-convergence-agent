{-# LANGUAGE DeriveGeneric #-}

import Prelude hiding (putStr)

import GHC.Generics

import Data.Text (Text)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions, encode)

import Data.ByteString.Lazy (ByteString, putStr)

data Person = Person {
  name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance ToJSON Person where
  -- No need to provide a toJSON implementation.

  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Person
  -- No need to provide a parseJSON implementation.

main :: IO ()
main = putStr $ encode (Person {name = "Joe", age = 12})
