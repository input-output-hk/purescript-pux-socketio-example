module App where

import Prelude
import App.SocketExample (Action(..), State, connectEvent, customDataEvent, customDataHandler, loginHandler, loginEvent, messageEvent, mkInitialState, update, userJoinedEvent, view, userJoinedHandler)
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Host, SocketIO, connect, on)
import DOM (DOM)
import Pux (App, CoreEffects, start, renderToDOM)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)

type AppEffects = (dom :: DOM, socket :: SocketIO)

hostname :: Host
hostname = "http://localhost:9999"

main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  actionChannel <- channel Closed
  let socketSignal = subscribe actionChannel :: Signal Action
  socket <- connect hostname
  on socket connectEvent \_ -> send actionChannel Connected
  on socket messageEvent \d -> send actionChannel (OnMessage d)
  on socket loginEvent $ loginHandler actionChannel
  on socket userJoinedEvent $ userJoinedHandler actionChannel
  on socket customDataEvent $ customDataHandler actionChannel

  app <- start
      { initialState: mkInitialState socket
      , update: update
      , view: view
      , inputs: [socketSignal]
      }
  renderToDOM "#app" app.html
  pure app
