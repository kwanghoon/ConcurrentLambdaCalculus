module Main where

import Data.List (nub)
import EvalActor (evaluateActor)
import qualified EvalActor as EA (Value(..))
import ActorExample (ActorExpr, actorIncrement, actorRendezvous, actorRelay, actorEcho, actorPing, actorThreeWayRelay, actorExamples)
import CTree (CTree(..), lower, NoEff)

-- Outcome type for exploration
data Outcome a = Done a | Stuck | Timeout deriving (Eq, Show)

-- Bounded exploration
explore :: Int -> CTree NoEff a -> [Outcome a]
explore fuel _ | fuel < 0 = [Timeout]
explore _ (Now v) = [Done v]
explore fuel (Choice p q) = explore fuel p ++ explore fuel q
explore _ Zero = [Stuck]
explore fuel (Later p) = explore (fuel - 1) p
explore _ (Eff op _) = case op of {}

-- Extract numeric values from Value
extractNum :: EA.Value -> Maybe Int
extractNum (EA.Num n) = Just n
extractNum _ = Nothing

-- Test a single actor example
testActorExample :: String -> ActorExpr -> Int -> IO ()
testActorExample name expr fuel = do
  putStrLn ("=== actor " ++ name ++ " ===")
  let tree = lower $ evaluateActor expr
      outcomes = explore fuel tree
      values = [v | Done v <- outcomes]
      numValues = nub (map extractNum values)
  putStrLn ("Outcomes: " ++ show outcomes)
  putStrLn ("Final values: " ++ show values)
  putStrLn ("Numeric results: " ++ show numValues)
  putStrLn ""

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "EvalActor Module Tests"
  putStrLn "============================================"
  putStrLn ""
  
  -- Test all actor examples with different fuel levels
  let fuel = 1000
  testActorExample "increment" actorIncrement fuel
  testActorExample "rendezvous" actorRendezvous fuel
  testActorExample "relay" actorRelay fuel
  testActorExample "echo" actorEcho fuel
  testActorExample "ping" actorPing fuel
  testActorExample "three-way-relay" actorThreeWayRelay fuel
  
  putStrLn "============================================"
  putStrLn "All tests completed"
  putStrLn "============================================"
