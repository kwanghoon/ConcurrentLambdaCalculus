{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Executable approximation of Sections 2, 4, and 7.
-- Haskell laziness does NOT statically enforce the paper's guardedness.
module ConcurrentLETREC.CTree
  ( CTree(..), CTreeC(..), Concurrent(..), NoEff
  , zeroC, choiceC, laterC, effectC, lower
  , parallel, parR, parRC, interp, interpSt, interpStC
  ) where

import Prelude hiding (interact)

-- A finite branching tree with explicit tau (Later) nodes.
data CTree e a where
  Now    :: a -> CTree e a
  Choice :: CTree e a -> CTree e a -> CTree e a
  Zero   :: CTree e a
  Eff    :: e b -> (b -> CTree e a) -> CTree e a
  Later  :: CTree e a -> CTree e a

instance Functor (CTree e) where
  fmap f x = x >>= (pure . f)
instance Applicative (CTree e) where
  pure = Now
  fs <*> xs = fs >>= \f -> fmap f xs
instance Monad (CTree e) where
  Now a >>= f = f a
  Choice p q >>= f = Choice (p >>= f) (q >>= f)
  Zero >>= _ = Zero
  Eff op k >>= f = Eff op (\v -> k v >>= f)
  Later p >>= f = Later (p >>= f)
instance MonadFail (CTree e) where
  fail _ = Zero

newtype CTreeC e a = CTreeC
  { runCTreeC :: forall r. (a -> CTree e r) -> CTree e r }

instance Functor (CTreeC e) where
  fmap f p = p >>= (pure . f)
instance Applicative (CTreeC e) where
  pure v = CTreeC (\k -> k v)
  f <*> x = f >>= \g -> fmap g x
instance Monad (CTreeC e) where
  p >>= f = CTreeC (\k -> runCTreeC p (\a -> runCTreeC (f a) k))
instance MonadFail (CTreeC e) where
  fail _ = zeroC

zeroC :: CTreeC e a
zeroC = CTreeC (\_ -> Zero)
choiceC :: CTreeC e a -> CTreeC e a -> CTreeC e a
choiceC p q = CTreeC (\k -> Choice (runCTreeC p k) (runCTreeC q k))
laterC :: CTreeC e a -> CTreeC e a
laterC p = CTreeC (\k -> Later (runCTreeC p k))
effectC :: e a -> CTreeC e a
effectC op = CTreeC (\k -> Eff op k)
lower :: CTreeC e a -> CTree e a
lower p = runCTreeC p Now

-- | A synchronising concurrent effect handler. An unmatched pair of
-- effects has no synchronised transition (Zero).
class Concurrent e where
  interact :: e a -> e b -> CTree e (a,b)

-- | Paper's (left-step) + (right-step) + (synchronisation) definition.
parallel :: Concurrent e => CTree e a -> CTree e b -> CTree e (a,b)
parallel p q = plus (left p q) (plus (right p q) (both p q))

-- Zero is the unit of nondeterministic choice up to bisimilarity.
-- Removing these dead branches makes exploration of small programs feasible.
plus :: CTree e a -> CTree e a -> CTree e a
plus Zero q = q
plus p Zero = p
plus p q = Choice p q

left :: Concurrent e => CTree e a -> CTree e b -> CTree e (a,b)
left (Choice p q) r = plus (left p r) (left q r)
left (Later p) q = Later (parallel p q)
left (Eff op k) q = Eff op (\v -> parallel (k v) q)
left _ _ = Zero

right :: Concurrent e => CTree e a -> CTree e b -> CTree e (a,b)
right p (Choice q r) = plus (right p q) (right p r)
right p (Later q) = Later (parallel p q)
right p (Eff op k) = Eff op (\v -> parallel p (k v))
right _ _ = Zero

both :: Concurrent e => CTree e a -> CTree e b -> CTree e (a,b)
both (Choice p q) r = plus (both p r) (both q r)
both p (Choice q r) = plus (both p q) (both p r)
both (Now v) (Now w) = Now (v,w)
both (Eff op k) (Eff op' k') = do
  (x,y) <- interact op op'
  parallel (k x) (k' y)
both _ _ = Zero

-- | Synchronous right-biased parallelism on ordinary choice trees.
parR :: Concurrent e => CTree e a -> CTree e b -> CTree e b
parR p q = fmap snd (parallel p q)

-- | Asynchronous right-biased parallelism on codensity choice trees.
-- Critical: the continuation k is supplied only to the right branch.
parRC :: Concurrent e => CTreeC e a -> CTreeC e b -> CTreeC e b
parRC p q = CTreeC (\k -> parR (lower p) (runCTreeC q k))

-- | Interpret an effect signature after synchronization has had a
-- chance to happen; the handler is applied *outside* parallelism.
interp :: (forall b. e b -> CTree f b) -> CTree e a -> CTree f a
interp _ (Now v) = Now v
interp h (Choice p q) = Choice (interp h p) (interp h q)
interp _ Zero = Zero
interp h (Later p) = Later (interp h p)
interp h (Eff op k) = h op >>= \v -> interp h (k v)

-- | Stateful global effect interpretation: handler returns the new
-- state, which is threaded through the whole nondeterministic tree.
interpSt :: s -> (forall b. s -> e b -> CTree f (b,s))
         -> CTree e a -> CTree f a
interpSt _ _ (Now v) = Now v
interpSt s h (Choice p q) = Choice (interpSt s h p) (interpSt s h q)
interpSt _ _ Zero = Zero
interpSt s h (Later p) = Later (interpSt s h p)
interpSt s h (Eff op k) = h s op >>= \(v,s') -> interpSt s' h (k v)

-- | Delimited codensity handler, as in Section 7.3.
interpStC :: s -> (forall b. s -> e b -> CTreeC f (b,s))
          -> CTreeC e a -> CTreeC f a
interpStC s h p = CTreeC $ \k ->
  interpSt s (\s' op -> lower (h s' op)) (lower p) >>= k

-- | No remaining externally visible effects.
data NoEff a where
