{-# LANGUAGE GADTs #-}
module ConcurrentLETREC.Channel
  ( Chan, ChanEff(..), send, receive, newChan, handleChannel ) where

import ConcurrentLETREC.CTree

type Chan = Int

data ChanEff a where
  SendInt    :: Chan -> Int -> ChanEff ()
  ReceiveInt :: Chan -> ChanEff Int
  NewChan    :: ChanEff Chan

instance Concurrent ChanEff where
  interact (SendInt c n) (ReceiveInt d) | c == d = Later (Now ((), n))
  interact (ReceiveInt c) (SendInt d n) | c == d = Later (Now (n, ()))
  interact _ _ = Zero

send :: Chan -> Int -> CTreeC ChanEff ()
send c n = effectC (SendInt c n)
receive :: Chan -> CTreeC ChanEff Int
receive c = effectC (ReceiveInt c)
newChan :: CTreeC ChanEff Chan
newChan = effectC NewChan

-- | A solitary send or receive is blocked (Zero). A fresh channel is
-- generated using the state, starting from 0 at the top-level.
handleChannel :: Chan -> ChanEff a -> CTreeC NoEff (a, Chan)
handleChannel n NewChan = pure (n,n+1)
handleChannel _ (SendInt _ _) = zeroC
handleChannel _ (ReceiveInt _) = zeroC
