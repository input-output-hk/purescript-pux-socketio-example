module Control.SocketIO.Client where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, Fn3, runFn1, runFn2, runFn3)

foreign import data SocketIO :: !
foreign import data Socket :: *

type Host = String
type Channel = String

type MsgCallback a eff b = a -> Eff (socket :: SocketIO | eff) b

foreign import connectImpl :: forall eff. Host -> (Eff (socket :: SocketIO | eff) Socket)
foreign import emitImpl :: forall d eff. Fn3 Socket Channel d (Eff (socket :: SocketIO | eff) Unit)
foreign import onImpl :: forall a eff. Fn3 Socket Channel (MsgCallback Channel eff a) (Eff (socket :: SocketIO | eff) Unit)

connect :: forall eff. Host -> Eff (socket :: SocketIO | eff) Socket
connect host = runFn1 connectImpl host

emit :: forall d eff. Socket -> Channel -> d -> Eff (socket :: SocketIO | eff) Unit
emit s ch da = runFn3 emitImpl s ch da

on :: forall a eff. Socket -> Channel -> (MsgCallback Channel eff a) -> Eff (socket :: SocketIO | eff) Unit
on s ch cb = runFn3 onImpl s ch cb
