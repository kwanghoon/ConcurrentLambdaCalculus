module Eval
  ( Value(..), Env(..), applyEnv, eval, evaluate ) where

import Syntax
import CTree
import Channel (ChanEff, send, receive, newChan, handleChannel)

-- | EOPL's extended environment. A recursive binding stores the source
-- function body, not a cyclic Haskell closure. Looking up f constructs a
-- closure capturing this very ExtendEnvRec node.
data Env
  = EmptyEnv
  | ExtendEnv Identifier Value Env
  | ExtendEnvRec Identifier Identifier Expr Env
  deriving (Eq, Show)

data Value
  = Num Int
  | Clo Identifier Expr Env
  deriving (Eq, Show)

applyEnv :: Env -> Identifier -> CTreeC e Value
applyEnv EmptyEnv _ = zeroC
applyEnv (ExtendEnv x v rest) y
  | x == y = pure v
  | otherwise = applyEnv rest y
applyEnv env@(ExtendEnvRec f arg body rest) y
  | f == y = pure (Clo arg body env)
  | otherwise = applyEnv rest y

eval :: Expr -> Env -> CTreeC ChanEff Value
eval (Val n) _ = pure (Num n)
eval (Add x y) env = do
  Num n <- eval x env
  Num m <- eval y env
  pure (Num (n + m))
eval (Var x) env = applyEnv env x
eval (Abs x body) env = pure (Clo x body env)
eval (App x y) env = do
  Clo arg body saved <- eval x env
  v <- eval y env
  laterC (eval body (ExtendEnv arg v saved))
-- EOPL let is semantically the call-by-value application (\x.body) rhs.
-- Using App here preserves its explicit Later step on both sides.
eval (Let x rhs body) env = eval (App (Abs x body) rhs) env
eval (LetRec f x fbody body) env =
  eval body (ExtendEnvRec f x fbody env)
eval (IfZero tst th el) env = do
  Num n <- eval tst env
  if n == 0 then eval th env else eval el env
eval (Send x y) env = do
  Num ch <- eval x env
  Num n <- eval y env
  send ch n
  pure (Num n)
eval (Receive x) env = do
  Num ch <- eval x env
  n <- receive ch
  pure (Num n)
eval NewChannel _ = do
  channel <- newChan
  pure (Num channel)
eval (Fork (Abs ch child)) env = do
  channel <- newChan
  parRC (eval child (ExtendEnv ch (Num channel) env))
        (pure (Num channel))
eval (Fork child) env = do
  channel <- newChan
  parRC (eval (App child (Val channel)) env)
        (pure (Num channel))

evaluate :: Expr -> CTreeC NoEff Value
evaluate x = interpStC 0 handleChannel (eval x EmptyEnv)
