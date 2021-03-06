module App.SocketExample where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Control.SocketIO.Client (Event, Socket, SocketIO, emit)
import DOM (DOM)
import Data.Array ((:), cons)
import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Pux (EffModel, noEffects)
import Pux.Html (Html, button, div, h1, h3, ul, li, text, input)
import Pux.Html.Attributes (disabled, style, value)
import Pux.Html.Events (onClick, onChange)
import Signal.Channel (CHANNEL, Channel, send)
import Prelude hiding (div)


-- state

type State =
    { socket :: Maybe Socket
    , connected :: Boolean
    , userToAdd :: Maybe String
    , userAdded :: Maybe String
    , usernames :: Array String
    , numUsers :: Int
    , messages :: Array String
    , messageToSend :: String
    , errors :: Array String
    , customDataMsg :: Maybe CustomDataMsg
    }

mkInitialState :: Socket -> State
mkInitialState s =
    { socket: Just s
    , connected: false
    , userToAdd: Nothing
    , userAdded: Nothing
    , usernames: []
    , numUsers: 0
    , messages: []
    , messageToSend: ""
    , errors: []
    , customDataMsg: Nothing
    }


-- actions

data Action
    = AddUser
    | OnUserLogin (Either MultipleErrors LoginMsg)
    | OnUserJoined (Either MultipleErrors UserJoinedMsg)
    | OnCustomData (Either MultipleErrors CustomDataMsg)
    | UserToAdd String
    | SendMessage String
    | SendCustomData
    | MessageToSend String
    | Connected
    | Closed
    | OnMessage String
    | NoOp


-- Socket

type ActionChannel = Channel Action


-- Chat

newtype LoginMsg = LoginMsg
    { numUsers :: Int
    }

derive instance genericLoginMsg :: Generic LoginMsg _
instance showUserLoginMsg :: Show LoginMsg where
  show x = genericShow x
instance isForeignLoginMsg :: IsForeign LoginMsg where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignLoginMsg :: AsForeign LoginMsg where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

newtype UserJoinedMsg = UserJoinedMsg
    { username :: String
    , numUsers :: Int
    }

derive instance genericUserJoinedMsg :: Generic UserJoinedMsg _
instance showUserJoinedMsg :: Show UserJoinedMsg where
  show x = genericShow x
instance isForeignUserJoinedMsg :: IsForeign UserJoinedMsg where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignUserJoinedMsg :: AsForeign UserJoinedMsg where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}

newtype CustomDataMsg = CustomDataMsg
    { from :: String
    , payload :: CustomData
    }

derive instance genericCustomDataMsg :: Generic CustomDataMsg _
instance showCustomDataMsg :: Show CustomDataMsg where
  show x = genericShow x
instance isForeignCustomDataMsg :: IsForeign CustomDataMsg where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignCustomDataMsg :: AsForeign CustomDataMsg where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}


newtype CustomData = CustomData
  { username :: String
  , message :: String
  , values :: Values
  }

derive instance genericCustomData :: Generic CustomData _
instance showCustomData :: Show CustomData where
  show x = genericShow x
instance isForeignCustomData :: IsForeign CustomData where
  read = readGeneric $ defaultOptions {unwrapSingleConstructors = true}
instance asForeignCustomData :: AsForeign CustomData where
  write = toForeignGeneric $ defaultOptions {unwrapSingleConstructors = true}


data Values =
    ValA
    | ValB
    | ValC


derive instance genericValues :: Generic Values _
instance showValues :: Show Values where
  show x = genericShow x
instance isForeignValues :: IsForeign Values where
  read = readGeneric $ defaultOptions
instance asForeignValues :: AsForeign Values where
  write = toForeignGeneric $ defaultOptions


-- events

connectEvent :: Event
connectEvent = "connect"

addUserEvent :: Event
addUserEvent = "add user"

loginEvent :: Event
loginEvent = "login"

userJoinedEvent :: Event
userJoinedEvent = "user joined"

messageEvent :: Event
messageEvent = "new message"

customDataEvent :: Event
customDataEvent = "custom data"


-- event handler

loginHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
loginHandler channel json =
    let result = runExcept $ read json in
    send channel $ OnUserLogin result

userJoinedHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
userJoinedHandler channel json =
    let result = runExcept $ read json in
    send channel $ OnUserJoined result

customDataHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
customDataHandler channel json =
    let result = runExcept $ read json in
    send channel $ OnCustomData result

-- update

update :: forall eff . Action -> State
    -> EffModel State Action (dom :: DOM, socket :: SocketIO | eff)
update AddUser state =
  { state: state { userToAdd = Nothing, userAdded = state.userToAdd }
  , effects: [ do
      _ <- case state.socket of
          Just socket ->
              case state.userToAdd of
                  Just user -> liftEff $ emit socket addUserEvent user
                  Nothing -> pure unit
          Nothing -> pure unit
      pure NoOp
    ]
  }
update (UserToAdd user) state =
    noEffects $ state { userToAdd = Just user }
update (SendMessage msg) state =
  { state: state { messageToSend = "" }
  , effects: [ do
      _ <- case state.socket of
          Just socket -> liftEff $ emit socket messageEvent msg
          Nothing -> pure unit
      pure NoOp
    ]
  }
update SendCustomData state =
  { state: state
  , effects: [ do
      _ <- case state.socket of
          Just socket ->
              let customData = CustomData
                              { username: fromMaybe "--" state.userAdded
                              , message: "hi"
                              , values: ValB
                              } in
              liftEff $ emit socket customDataEvent $ write customData
          Nothing -> pure unit
      pure NoOp
    ]
  }
update (MessageToSend msg) state =
    noEffects $ state { messageToSend = msg }
update Connected state =
    noEffects $ state { connected = true }
update (OnUserLogin (Right (LoginMsg loginMsg))) state =
    noEffects $ state { numUsers = loginMsg.numUsers }
update (OnUserLogin (Left error)) state =
    noEffects $ state { errors = (show error) : state.errors }
update (OnUserJoined (Right (UserJoinedMsg joinedMsg))) state =
    noEffects $ state { usernames = joinedMsg.username : state.usernames
                      , numUsers = joinedMsg.numUsers
                      }
update (OnUserJoined (Left error)) state =
    noEffects $ state { errors = (show error) : state.errors }
update (OnCustomData (Right customDataMsg)) state =
    noEffects $ state { customDataMsg = Just customDataMsg }
update (OnCustomData (Left error)) state =
    noEffects $ state { errors = (show error) : state.errors }
update (OnMessage msg) state =
    noEffects $ state { messages = msg : state.messages }
update Closed state =
    noEffects $ state { connected = false }
update _ state =
    noEffects $ state


-- views

view :: State -> Html Action
view state =
  let
  hiddenStyle = Tuple "display" "none"
  emptyStyle = Tuple "" ""
  hiddenStyleUserAdded = if isJust state.userAdded then hiddenStyle else emptyStyle
  hiddenStyleUserNotAdded = if isNothing state.userAdded then hiddenStyle else emptyStyle in
  div []
    [ h1 [] [ text "PureScript + socket.io example" ]
    , h3 [] [ text $ "connected: " <> show state.connected ]
    , h3 [] [ text <<< (<>) "no. users: " <<< show $ state.numUsers ]
    , div [ style [hiddenStyleUserAdded] ]
          [ input
                [ onChange (UserToAdd <<< _.target.value)
                , disabled $ isJust state.userAdded
                , value $ fromMaybe "" state.userToAdd ]
                []
          , button
              [ onClick $ const AddUser
              , disabled $ isNothing state.userToAdd || isJust state.userAdded ]
              [ text addUserEvent ]
          ]
    , div [ style [hiddenStyleUserNotAdded] ]
        [
            div []
                  [ input
                      [ onChange (MessageToSend <<< _.target.value)
                      , disabled $ isNothing state.userAdded
                      , value state.messageToSend ]
                      []
                  ,  button
                        [ onClick (const $ SendMessage state.messageToSend)
                        , disabled $ null state.messageToSend || isNothing state.userAdded ]
                        [ text messageEvent ]
                  ]
            , button
                  [ onClick $ const SendCustomData
                  , disabled $ isNothing state.userAdded ]
                  [ text $ "send " <> customDataEvent ]
            , h3 [] [ text $ "users joined: " <> show state.usernames ]
            , h3 [] [ text $ "custom data: " <> case state.customDataMsg of
                                                    Just d -> show d
                                                    Nothing -> ""
                    ]
            , ul [] <<< cons (listHeaderView "messages: ") $ map listItemView state.messages
        ]
    , ul [] <<< cons (listHeaderView "errors: ") $ map listItemView state.errors
    ]

listHeaderView :: String -> Html Action
listHeaderView title =
    li [] [ h3 [] [text title] ]

listItemView :: String -> Html Action
listItemView message =
    li [] [ text message ]
