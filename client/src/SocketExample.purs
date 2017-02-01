module App.SocketExample where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.SocketIO.Client (Event, Socket, SocketIO, emit)
import DOM (DOM)
import Data.Argonaut (jsonEmptyArray)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?)) as A
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceAny, traceAnyM)
import Pux (EffModel, noEffects)
import Pux.Html (Html, button, div, h1, h3, ul, li, text, p)
import Pux.Html.Events (onClick)
import Signal.Channel (CHANNEL, Channel, send)
import Prelude hiding (div)


-- state

type State =
    { socket :: Maybe Socket
    , connected :: Boolean
    , username :: Maybe String
    , usernames :: Array String
    , numUsers :: Int
    , messages :: Array String
    , errors :: Array String
    }

mkInitialState :: Socket -> State
mkInitialState s =
    { socket: Just s
    , connected: false
    , username: Nothing
    , usernames: []
    , numUsers: 0
    , messages: []
    , errors: []
    }


-- actions

data Action
    = AddUser String
    | OnUserLogin (Either String LoginMsg)
    | OnUserJoined (Either String UserJoinedMsg)
    | SendMessage String
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

newtype UserJoinedMsg = UserJoinedMsg
    { username :: String
    , numUsers :: Int
    }

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

-- event handler

loginHandler :: forall eff. ActionChannel -> Either String A.Json
    -> Eff (channel :: CHANNEL | eff) Unit
loginHandler channel json =
    let result = decodeLoginMsg json in
    send channel (OnUserLogin result)

userJoinedHandler :: forall eff. ActionChannel -> Either String A.Json
    -> Eff (channel :: CHANNEL | eff) Unit
userJoinedHandler channel json =
    let result = decodeUserJoined json in
    send channel (OnUserJoined result)

-- Json

instance decodeJsonLoginMsg :: A.DecodeJson LoginMsg where
  decodeJson json = do
    obj <- A.decodeJson json
    numUsers <- obj A..? "numUsers"
    pure $ LoginMsg { numUsers }

decodeLoginMsg :: Either String A.Json -> Either String LoginMsg
decodeLoginMsg json =
    let decode json' = A.decodeJson json' in
    either (Left <<< show) decode json

instance decodeJsonUserJoinedMsg :: A.DecodeJson UserJoinedMsg where
  decodeJson json = do
    obj <- A.decodeJson json
    username <- obj A..? "username"
    numUsers <- obj A..? "numUsers"
    pure $ UserJoinedMsg { username, numUsers }

decodeUserJoined :: Either String A.Json -> Either String UserJoinedMsg
decodeUserJoined json =
    let decode json' = A.decodeJson json' in
    either (Left <<< show) decode json

-- update

update :: forall eff . Action -> State
    -> EffModel State Action (dom :: DOM, socket :: SocketIO | eff)
update (AddUser name) state =
  { state: state { username = Just name }
  , effects: [ do
      _ <- case state.socket of
          Just socket -> liftEff $ emit socket addUserEvent name
          Nothing -> pure unit
      pure NoOp
    ]
  }
update (SendMessage msg) state =
  { state: state
  , effects: [ do
      _ <- case state.socket of
          Just socket -> liftEff $ emit socket messageEvent msg
          Nothing -> pure unit
      pure NoOp
    ]
  }
update Connected state =
    noEffects $ state { connected = true }
update (OnUserLogin (Right (LoginMsg loginMsg))) state =
    noEffects $ state { numUsers = loginMsg.numUsers }
update (OnUserLogin (Left error)) state =
    noEffects $ state { errors = error : state.errors }
update (OnUserJoined (Right (UserJoinedMsg joinedMsg))) state =
    noEffects $ state { usernames = joinedMsg.username : state.usernames
                      , numUsers = joinedMsg.numUsers
                      }
update (OnUserJoined (Left error)) state =
    noEffects $ state { errors = error : state.errors }
update (OnMessage msg) state =
    noEffects $ state { messages = msg : state.messages }
update Closed state =
    noEffects $ state { connected = false }
update _ state =
    noEffects $ state


-- views

view :: State -> Html Action
view state =
  div []
    [ h1
        []
        [ text "PureScript + socket.io example" ]
    , h3
        []
        [ text $ "connected: " <> show state.connected ]
    , div
        []
        [ button
            [ onClick (const $ AddUser "jk") ]
            [ text addUserEvent ]
    ]
    , div
        []
        [ button
            [ onClick (const $ SendMessage "next message") ]
            [ text messageEvent ]
    ]
    , h3
        []
        -- I know, accessing nested newtypes looks not nice with this ...
        [ text <<< (<>) "no. users: " <<< show $ state.numUsers ]
    , h3
        []
        [ text $ "errors: " ]
    , ul
        []
        $ map listItemView state.errors
    , h3
        []
        [ text $ "names: " ]
    , p
        []
        [ text $ show state.usernames ]
    , h3
        []
        [ text $ "messages: " ]
    , ul
        []
        $ map listItemView state.messages
]

listItemView :: String -> Html Action
listItemView message =
    li
        []
        [ text message ]
