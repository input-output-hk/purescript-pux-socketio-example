module Control.SocketIO.Client where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3, runFn1, runFn3)

foreign import data SocketIO :: !
foreign import data Socket :: *

newtype Host = Host String
newtype Channel = Channel String
newtype Event = Event String

type EventHandler a b eff = a -> Eff (socket :: SocketIO | eff) b

foreign import connectImpl :: forall eff. Host -> (Eff (socket :: SocketIO | eff) Socket)
foreign import emitImpl :: forall d eff. Fn3 Socket Channel d (Eff (socket :: SocketIO | eff) Unit)
foreign import onImpl :: forall a b eff. Fn3 Socket Event (EventHandler a b eff) (Eff (socket :: SocketIO | eff) Unit)

connect :: forall eff. Host -> Eff (socket :: SocketIO | eff) Socket
connect host = runFn1 connectImpl host

emit :: forall d eff. Socket -> Channel -> d -> Eff (socket :: SocketIO | eff) Unit
emit s ch d = runFn3 emitImpl s ch d

on :: forall a b eff. Socket -> Event -> (EventHandler a b eff) -> Eff (socket :: SocketIO | eff) Unit
on s e cb = runFn3 onImpl s e cb
