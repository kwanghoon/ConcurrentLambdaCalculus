module Machine
  ( Code(..), ValueM(..), EnvM(..), Elem(..), Stack, Conf
  , compile, comp, applyEnvM, exec, execute, conv, convEnv, valueOf ) where

import Syntax
import Eval (Value(..), Env(..))
import CTree
import Channel

-- | Named-variable variant of Fig. 4; LETREC is the sole new instruction
-- for recursion. The existing return frame and RET restore lexical scope.
data Code
  = PUSH Int Code | ADD Code | ISNUM Code | LOOKUP Identifier Code
  | ABS Identifier Code Code | RET | ISCLO Code | APP Code
  | SEND Code | RECEIVE Code | FORK Identifier Code Code | HALT
  | IFZERO Code Code
  | LETREC Identifier Identifier Code Code Code
  deriving (Eq, Show)

-- | The machine's extended environment mirrors the interpreter's.
data EnvM
  = EmptyEnvM
  | ExtendEnvM Identifier ValueM EnvM
  | ExtendEnvRecM Identifier Identifier Code EnvM
  deriving (Eq, Show)

data ValueM = NumM Int | CloM Identifier Code EnvM
  deriving (Eq, Show)

data Elem = VAL ValueM | CLO Code EnvM
  deriving (Eq, Show)

type Stack = [Elem]
type Conf = (Stack, EnvM)

compile :: Expr -> Code
compile e = comp e HALT

comp :: Expr -> Code -> Code
comp (Val n) c = PUSH n c
comp (Add x y) c = comp x (ISNUM (comp y (ADD c)))
comp (Var x) c = LOOKUP x c
comp (Abs x body) c = ABS x (comp body RET) c
comp (App x y) c = comp x (ISCLO (comp y (APP c)))
comp (Let x rhs body) c = comp (App (Abs x body) rhs) c
-- Body runs with a saved continuation/environment frame and ends at RET.
-- This is NOT the fixpoint-combinator translation; f is bound by EnvRecM.
comp (LetRec f x fbody body) next =
  LETREC f x (comp fbody RET) (comp body RET) next
comp (IfZero tst th el) c = comp tst (IFZERO (comp th c) (comp el c))
comp (Send x y) c = comp x (ISNUM (comp y (SEND c)))
comp (Receive x) c = comp x (RECEIVE c)
comp (Fork ch child) c = FORK ch (comp child HALT) c

applyEnvM :: EnvM -> Identifier -> CTreeC e ValueM
applyEnvM EmptyEnvM _ = zeroC
applyEnvM (ExtendEnvM x v rest) y
  | x == y = pure v
  | otherwise = applyEnvM rest y
applyEnvM env@(ExtendEnvRecM f x fcode rest) y
  | f == y = pure (CloM x fcode env)
  | otherwise = applyEnvM rest y

exec :: Code -> Conf -> CTreeC ChanEff Conf
exec (PUSH n c) (s,e) = exec c (VAL (NumM n):s,e)
exec (ADD c) (VAL (NumM m):VAL (NumM n):s,e) =
  exec c (VAL (NumM (n+m)):s,e)
exec (ISNUM c) conf@(VAL (NumM _):_,_) = exec c conf
exec (LOOKUP x c) (s,e) = do
  v <- applyEnvM e x
  exec c (VAL v:s,e)
exec (ABS x body c) (s,e) = exec c (VAL (CloM x body e):s,e)
exec RET (VAL v:CLO c saved:s,_) = exec c (VAL v:s,saved)
exec (ISCLO c) conf@(VAL (CloM _ _ _):_,_) = exec c conf
exec (APP c) (VAL v:VAL (CloM x body saved):s,e) =
  laterC (exec body (CLO c e:s,ExtendEnvM x v saved))
exec (SEND c) (VAL (NumM n):VAL (NumM ch):s,e) = do
  send ch n
  exec c (VAL (NumM n):s,e)
exec (RECEIVE c) (VAL (NumM ch):s,e) = do
  n <- receive ch
  exec c (VAL (NumM n):s,e)
exec (FORK x child parent) (s,e) = do
  ch <- newChan
  parRC (exec child ([],ExtendEnvM x (NumM ch) e))
        (exec parent (VAL (NumM ch):s,e))
exec (IFZERO th el) (VAL (NumM n):s,e) =
  exec (if n == 0 then th else el) (s,e)
exec (LETREC f x fcode body next) (s,e) =
  exec body (CLO next e:s,ExtendEnvRecM f x fcode e)
exec HALT conf = pure conf
exec _ _ = zeroC

execute :: Code -> Conf -> CTreeC NoEff Conf
execute code conf = interpStC 0 handleChannel (exec code conf)

-- | Source-to-machine closure translation used when formulating the
-- compiler correctness property. It is not required by the runtime.
conv :: Value -> ValueM
conv (Num n) = NumM n
conv (Clo x body env) = CloM x (comp body RET) (convEnv env)

convEnv :: Env -> EnvM
convEnv EmptyEnv = EmptyEnvM
convEnv (ExtendEnv x v rest) = ExtendEnvM x (conv v) (convEnv rest)
convEnv (ExtendEnvRec f x body rest) =
  ExtendEnvRecM f x (comp body RET) (convEnv rest)

valueOf :: Conf -> Maybe Int
valueOf (VAL (NumM n):_,_) = Just n
valueOf _ = Nothing
