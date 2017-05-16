module SPAKE2 (start, finish) where

import Data.Text.Lazy (Text)
import Data.ByteString.Lazy (ByteString, empty)

start :: Text -> Text -> ByteString
start _ _ = empty

finish :: ByteString -> Text -> ByteString
-- body side
finish _ _ = empty
