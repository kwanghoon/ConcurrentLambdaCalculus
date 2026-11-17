module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Syntax
import Eval
import qualified EvalActor
import Machine
import Runner
import Examples
import Actor2Channel
import CTree
import ConcurrencyExampleRun (main)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["concurrency-example"] -> ConcurrencyExampleRun.main
    ["list"] -> mapM_ (putStrLn . fst) examples
    ["actor-list"] -> mapM_ (putStrLn . fst) actorExamples
    ["actor-translations"] -> printActorTranslations
    ["actor", name] -> runActorExample name 100
    ["actor", name, f] -> case readMaybe f of
      Just fuel -> runActorExample name fuel
      Nothing -> putStrLn "Fuel must be an integer"
    ["actor-eval", name] -> runActorExampleWithEvalActor name 100
    ["actor-eval", name, f] -> case readMaybe f of
      Just fuel -> runActorExampleWithEvalActor name fuel
      Nothing -> putStrLn "Fuel must be an integer"
    [name] -> runExample name 100
    [name,f] -> case readMaybe f of
      Just fuel -> runExample name fuel
      Nothing -> putStrLn "Fuel must be an integer"
    _ -> putStrLn "Usage: cabal run concurrent-letrec -- list | concurrency-example | EXAMPLE [TAU-FUEL] | actor-list | actor-translations | actor EXAMPLE [TAU-FUEL] | actor-eval EXAMPLE [TAU-FUEL]"

runExample :: String -> Int -> IO ()
runExample name fuel = case lookup name examples of
  Nothing -> putStrLn ("Unknown example: " ++ name ++ "; use list")
  Just expr -> case checkClosed expr of
    Left e -> putStrLn ("scope error: " ++ e)
    Right () -> do
      putStrLn ("=== " ++ name ++ " ===")
      putStrLn ("Expr: " ++ show expr)
      putStrLn ("Compiled code: " ++ show (compile expr))
      putStrLn ("Source outcomes: " ++ show (map summarizeSource (sourceOutcomes fuel expr)))
      putStrLn ("VM outcomes: " ++ show (map summarizeMachine (machineOutcomes fuel expr)))
      putStrLn ("Numeric agreement (bounded): " ++ show (compareInts fuel expr))

runActorExample :: String -> Int -> IO ()
runActorExample name fuel = case lookup name actorExamples of
  Nothing -> putStrLn ("Unknown actor example: " ++ name ++ "; use actor-list")
  Just expr -> case checkClosed (actorToChannel expr) of
    Left e -> putStrLn ("scope error: " ++ e)
    Right () -> do
      putStrLn ("=== actor " ++ name ++ " ===")
      putStrLn ("Actor expr: " ++ prettyActorProgram expr)
      putStrLn ("Channel encoding: " ++ prettyChannelProgram (actorToChannel expr))
      putStrLn ("Translation: " ++ actorTranslationSummary expr)
      putStrLn ("Compiled code: " ++ show (compile (actorToChannel expr)))
      putStrLn ("Source outcomes: " ++ show (map summarizeSource (sourceOutcomes fuel (actorToChannel expr))))

runActorExampleWithEvalActor :: String -> Int -> IO ()
runActorExampleWithEvalActor name fuel = case lookup name actorExamples of
  Nothing -> putStrLn ("Unknown actor example: " ++ name ++ "; use actor-list")
  Just expr -> do
      putStrLn ("=== actor-eval " ++ name ++ " ===")
      putStrLn ("Actor expr: " ++ prettyActorProgram expr)
      let allOutcomes = explore fuel (lower (EvalActor.evaluateActor expr))
          outcomes = filter (/= Stuck) allOutcomes
      putStrLn ("All outcomes: " ++ show (map summarizeEvalActorOutcome allOutcomes))
      putStrLn ("EvalActor outcomes (non-stuck): " ++ show (map summarizeEvalActorOutcome outcomes))

printActorTranslations :: IO ()
printActorTranslations =
  mapM_ (\(name, (before, after)) -> do
    putStrLn ("=== " ++ name ++ " ===")
    putStrLn ("Actor: " ++ before)
    putStrLn ("Channel:  " ++ after)) actorTranslationPairs

summarizeSource :: Outcome Value -> String
summarizeSource (Done (Num n)) = "Num " ++ show n
summarizeSource (Done (Clo _ _ _)) = "<closure>"
summarizeSource Stuck = "stuck"
summarizeSource Timeout = "timeout"

summarizeEvalActorOutcome :: Outcome EvalActor.Value -> String
summarizeEvalActorOutcome (Done (EvalActor.Num n)) = "Num " ++ show n
summarizeEvalActorOutcome (Done (EvalActor.Clo _ _ _)) = "<closure>"
summarizeEvalActorOutcome Stuck = "stuck"
summarizeEvalActorOutcome Timeout = "timeout"

summarizeMachine :: Outcome Conf -> String
summarizeMachine (Done c) = case valueOf c of
  Just n -> "Num " ++ show n
  Nothing -> "<non-numeric or empty stack>"
summarizeMachine Stuck = "stuck"
summarizeMachine Timeout = "timeout"
