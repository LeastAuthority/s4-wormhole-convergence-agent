module SPAKE2 (start, finish) where

import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString, empty)

start :: Text -> Text -> ByteString
start _ _ = empty

finish :: Text -> Text -> ByteString
finish _ _ = empty
