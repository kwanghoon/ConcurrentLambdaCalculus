module Main where

import Data.List (nub)
import EvalActor (evaluateActor, Value(..))
import ActorExample (ActorExpr(..), actorIncrement, actorRendezvous, actorRelay, actorEcho, actorPing, actorThreeWayRelay)
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

-- Extract final values
getResults :: Int -> CTree NoEff Value -> [Value]
getResults fuel tree = [v | Done v <- explore fuel tree, v /= stuck]
  where
    stuck = undefined

-- Helper to run a EvalActor computation
runEvalActor :: String -> ActorExpr -> Int -> IO ()
runEvalActor name expr fuel = do
  let tree = lower $ evaluateActor expr
      outcomes = explore fuel tree
      values = [v | Done v <- outcomes]
  putStrLn $ name ++ ":"
  putStrLn $ "  Outcomes: " ++ show outcomes
  putStrLn $ "  Final values: " ++ show values
  putStrLn ""

-- Print a test header
testHeader :: String -> IO ()
testHeader name = putStrLn $ "\n=== Test: " ++ name ++ " ==="

-- Test 1: actorIncrement
test1 :: IO ()
test1 = do
  testHeader "actorIncrement"
  putStrLn "Expected: Num 42"
  runEvalActor "actorIncrement" actorIncrement 100

-- Test 2: actorRendezvous
test2 :: IO ()
test2 = do
  testHeader "actorRendezvous"
  putStrLn "Expected: Num 10"
  runEvalActor "actorRendezvous" actorRendezvous 100

-- Test 3: actorRelay
test3 :: IO ()
test3 = do
  testHeader "actorRelay"
  putStrLn "Expected: Num 5 (3 + 2)"
  runEvalActor "actorRelay" actorRelay 100

-- Test 4: actorEcho
test4 :: IO ()
test4 = do
  testHeader "actorEcho"
  putStrLn "Expected: Num 7"
  runEvalActor "actorEcho" actorEcho 100

-- Test 5: actorPing
test5 :: IO ()
test5 = do
  testHeader "actorPing"
  putStrLn "Expected: Num 11"
  runEvalActor "actorPing" actorPing 100

-- Test 6: actorThreeWayRelay
test6 :: IO ()
test6 = do
  testHeader "actorThreeWayRelay"
  putStrLn "Expected: Num 13 (7 + 1 + 2 + 3)"
  runEvalActor "actorThreeWayRelay" actorThreeWayRelay 100

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "EvalActor Module Tests"
  putStrLn "============================================"
  test1
  test2
  test3
  test4
  test5
  test6
  putStrLn "============================================"
  putStrLn "All tests completed"
  putStrLn "============================================"
