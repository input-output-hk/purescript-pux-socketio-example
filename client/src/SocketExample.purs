module App.SocketExample where

import Prelude
import Pux (EffModel, noEffects)
import Pux.Html (Html, h1, h3, div, button, text)
import Pux.Html.Events (onClick)
import Control.SocketIO.Client (Socket, SocketIO, emit)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)

data Action
    = Emit String
    | Connect
    | NoOp

newtype State = State {
    socket :: Maybe Socket
}

initialState :: State
initialState = State {
    socket: Nothing
}

update :: forall eff . Action -> State -> EffModel State Action (dom :: DOM, socket :: SocketIO | eff)
update (Emit msg) (State state) =
    -- noEffects $ state
  { state: (State state)
  , effects: [ do
      _ <- case state.socket of
          Just s -> liftEff $ emit s "new message" "my msg2"
          Nothing -> pure unit
      pure NoOp
    ]
  }

update Connect state =
    noEffects $ state

update NoOp state =
    noEffects $ state

view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text "PureScript + socket.io example" ]
    , div []
      [ button [ onClick (const $ Connect) ] [ text "connect" ]
      , button [ onClick (const $ Emit "my message") ] [ text "emit" ]
    ]
]
