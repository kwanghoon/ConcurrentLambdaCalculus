{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
module Runner
  ( Outcome(..), explore, sourceOutcomes, machineOutcomes
  , sourceInts, machineInts, compareInts ) where

import Data.List (nub)
import CTree
import Syntax
import Eval
import Machine

-- | Bounded exploration, not bisimilarity or a complete model checker.
-- τ transitions consume fuel; choice nodes do not.
data Outcome a = Done a | Stuck | Timeout deriving (Eq, Show)

explore :: Int -> CTree NoEff a -> [Outcome a]
explore fuel _ | fuel < 0 = [Timeout]
explore _ (Now v) = [Done v]
explore fuel (Choice p q) = explore fuel p ++ explore fuel q
explore _ Zero = [Stuck]
explore fuel (Later p) = explore (fuel - 1) p
explore _ (Eff op _) = case op of {}

sourceOutcomes :: Int -> Expr -> [Outcome Value]
sourceOutcomes fuel = filter (/= Stuck) . explore fuel . lower . evaluate

machineOutcomes :: Int -> Expr -> [Outcome Conf]
machineOutcomes fuel expr =
  filter (/= Stuck)
    (explore fuel (lower (execute (compile expr) ([],EmptyEnvM))))

sourceInts :: Int -> Expr -> [Int]
sourceInts fuel expr = nub [n | Done (Num n) <- sourceOutcomes fuel expr]

machineInts :: Int -> Expr -> [Int]
machineInts fuel expr = nub
  [n | Done conf <- machineOutcomes fuel expr, Just n <- [valueOf conf]]

compareInts :: Int -> Expr -> Bool
compareInts fuel expr =
  let src = sourceInts fuel expr
      tgt = machineInts fuel expr
  in all (`elem` tgt) src && all (`elem` src) tgt
