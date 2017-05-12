module ListNameplates (listNameplates) where

import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

data State = Done | Starting | Bound

listNameplates :: WebSockets.ClientApp ()
listNameplates conn =
  WebSockets.receiveData conn >>= (
  \msg ->
    case (msg) of
      Model.UndecodableMessage -> putStrLn "boo"
      Model.Welcome server_tx  -> putStrLn "welcome"
  )
