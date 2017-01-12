module App.SocketExample where

import Control.Monad.Eff.Class (liftEff)
import Control.SocketIO.Client (Channel(..), Socket, SocketIO, emit)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Pux (EffModel, noEffects)
import Pux.Html (Html, button, div, h1, h3, text)
import Pux.Html.Events (onClick)
import Prelude hiding (div)

data Action
    = Emit String
    | Connected
    | Closed
    | OnMessage String
    | NoOp

newtype State = State {
    socket :: Maybe Socket
  , connected :: Boolean
  , message :: String
}


mkInitialState :: Socket -> State
mkInitialState s = State {
    socket: Just s
  , connected: false
  , message: ""
}


update :: forall eff . Action -> State -> EffModel State Action (dom :: DOM, socket :: SocketIO | eff)
update (Emit msg) (State state) =
  { state: (State state)
  , effects: [ do
      _ <- case state.socket of
          Just s -> liftEff $ emit s (Channel "new message") "my msg"
          Nothing -> pure unit
      pure NoOp
    ]
  }

update Connected (State state) =
    noEffects $ State $ state { connected = true }

update Closed (State state) =
    noEffects $ State state { connected = false }

update (OnMessage msg) (State state) =
    noEffects <<< State $ state { message = msg }

update _ state =
    noEffects $ state


view :: State -> Html Action
view (State state) =
  div []
    [ h1 [] [ text "PureScript + socket.io example" ]
    ,  h3 [] [ text $ "connected: " <> show state.connected ]
    ,  h3 [] [ text $ "message: " <> state.message ]
    , div []
      [ button [ onClick (const $ Emit "my message") ] [ text "emit" ]
    ]
]
