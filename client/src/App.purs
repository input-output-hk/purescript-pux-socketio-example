module App where

import App.SocketExample (Action(OnMessage, Connected, Closed), State, mkInitialState, update, view)
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Event(..), Host(..), SocketIO, connect, on)
import DOM (DOM)
import Prelude (($), bind, pure)
import Pux (App, CoreEffects, start, renderToDOM)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)

type AppEffects = (dom :: DOM, socket :: SocketIO)

main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  socketCh <- channel Closed
  let socketSignal = subscribe socketCh :: Signal Action
  socket <- connect $ Host "http://localhost:9999"
  on socket (Event "connect") \d -> send socketCh Connected
  on socket (Event "new message") \d -> send socketCh (OnMessage d)

  app <- start
    { initialState: mkInitialState socket
    , update: update
    , view: view
    , inputs: [socketSignal]
    }
  renderToDOM "#app" app.html
  pure app
