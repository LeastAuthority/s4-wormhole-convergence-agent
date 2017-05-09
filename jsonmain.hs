import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL

data Message = Message { foo :: String } deriving (Show, Eq)

main :: IO ()
main =
  let doc = "{\"foo\": \"bar\"}"
      obj = json doc :: Message
  in
    putStrLn obj
