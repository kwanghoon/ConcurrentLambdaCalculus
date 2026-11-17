{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module EvalActor
  ( evaluateActor, Value(..), Env(..) ) where

import ActorSyntax (ActorExpr(..), Identifier)
import CTree (CTreeC, NoEff, interpStC, laterC, parRC, zeroC)
import Channel (ChanEff, handleChannel, send, receive, newChan)

data Env
  = EmptyEnv
  | ExtendEnv Identifier Value Env
  | ExtendEnvRec Identifier Identifier ActorExpr Env
  deriving (Eq, Show)

data Value
  = Num Int
  | Clo Identifier ActorExpr Env
  deriving (Eq, Show)

applyEnv :: Env -> Identifier -> CTreeC eff Value
applyEnv EmptyEnv _ = zeroC
applyEnv (ExtendEnv x v rest) y
  | x == y = pure v
  | otherwise = applyEnv rest y
applyEnv env@(ExtendEnvRec f arg body rest) y
  | f == y = do
      selfVal <- applyEnv env "self"
      case selfVal of
        Num _ -> pure (Clo arg body env)
        _ -> zeroC
  | otherwise = applyEnv rest y  

-- | Evaluate an actor program by partially evaluating the composition:
-- evaluateActor = evaluate . actorToChannel
--
-- This is done by case analysis on ActorExpr, unfolding actorToChannel
-- and then evaluate for each case.
evaluateActor :: ActorExpr -> CTreeC NoEff Value
evaluateActor expr = evalActorWith "self" expr

evalActorWith :: Identifier -> ActorExpr -> CTreeC NoEff Value
evalActorWith self ae = interpStC 0 handleChannel (
  do channel <- newChan
     let env = ExtendEnv self (Num channel) EmptyEnv
     evalActor self ae env)

evalActor :: Identifier -> ActorExpr -> Env -> CTreeC ChanEff Value
evalActor _ (AVal n) _ = pure (Num n)

evalActor self (AAdd x y) env = do
  Num n <- evalActor self x env
  Num m <- evalActor self y env
  pure (Num (n + m))

evalActor _ (AVar x) env = applyEnv env x

evalActor self ASelf env = applyEnv env self

evalActor self (AAbs x body) env = pure (Clo x body env)

evalActor self (AApp f x) env = do
  Clo arg body saved <- evalActor self f env
  v <- evalActor self x env
  laterC (evalActor self body (ExtendEnv arg v saved))

evalActor self (ALet x rhs body) env =
  evalActor self (AApp (AAbs x body) rhs) env

evalActor self (ALetRec f x fbody body) env =
  evalActor self body (ExtendEnvRec f x fbody env)

evalActor self (AIfZero tst th el) env = do
  Num n <- evalActor self tst env
  if n == 0 then evalActor self th env else evalActor self el env

evalActor self (ASend ch v) env = do
  Num c <- evalActor self ch env
  Num n <- evalActor self v env
  send c n
  pure (Num n)

evalActor self AReceive env = do
  c <- applyEnv env self
  case c of
    Num ch -> Num <$> receive ch
    _ -> zeroC

evalActor _ (ASpawn (AAbs x body)) env = do
  channel <- newChan
  parRC (evalActor x body (ExtendEnv x (Num channel) env))
        (pure (Num channel))

evalActor _ (ASpawn body) env = do
  let fresh = "self"
  channel <- newChan
  parRC (evalActor fresh body (ExtendEnv fresh (Num channel) env))
        (pure (Num channel))


