module App where

import Control.Monad.Eff (Eff)
import Prelude(bind, pure)
import Pux (App, Update, CoreEffects, start, fromSimple, renderToDOM)
import App.SocketExample (State(..), Action, update, view)
import Control.SocketIO.Client (SocketIO, connect)
import Data.Maybe (Maybe(..))
import DOM (DOM)

type AppEffects = (dom :: DOM, socket :: SocketIO)

main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  socket <- connect "http://localhost:9999"
  app <- start
    { initialState: State { socket: Just socket }
    , update: update :: Update State Action AppEffects
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.html
  pure app
