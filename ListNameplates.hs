import qualified Network.WebSockets as WebSockets

import qualified MagicWormholeModel as Model

data State = Done | Starting | Bound

listNameplates :: WebSockets.ClientApp ()
listNameplates conn = execute (Starting, []) conn

processMessage :: State -> Model.Message -> (State, [Model.Message])
processMessage state (Model.Error message original) = do
  print "Oh no an error"
  print message
  print original
  return (Done, [])

processMessage Starting (Model.Welcome server_tx) = do
  print "server says hi"
  return (Starting [Model.Bind "appid" "alice"])

processMessage Starting (Model.Ack server_tx id) = do
  print "server acked"
  print id
  return (Bound, [Model.List])

processMessage Bound (Model.Nameplates nameplates) = do
  print nameplates
  return (Done, [])
