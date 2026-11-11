module Main where

import Control.Monad (unless)
import Syntax
import Runner
import Machine (Code(..), comp)
import Examples
import Actor2Channel

assert :: String -> Bool -> IO ()
assert name ok = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  unless ok (error ("test failed: " ++ name))

checkInt :: String -> Int -> Int -> Expr -> IO ()
checkInt label fuel expected expr = do
  assert (label ++ " interpreter") (sourceInts fuel expr == [expected])
  assert (label ++ " compiler+VM") (machineInts fuel expr == [expected])
  assert (label ++ " bounded agreement") (compareInts fuel expr)

main :: IO ()
main = do
  checkInt "addition" 30 5 (Add (Val 2) (Val 3))
  checkInt "lexical closure" 40 12
    (Let "x" (Val 7) (App (Abs "y" (Add (Var "x") (Var "y"))) (Val 5)))
  checkInt "ifzero" 30 11 (IfZero (Val 0) (Val 11) (Val 22))
  checkInt "shadowing" 40 9 (Let "x" (Val 1) (Let "x" (Val 9) (Var "x")))
  checkInt "captured lexical variable" 60 12
    (Let "x" (Val 7)
      (Let "f" (Abs "y" (Add (Var "x") (Var "y")))
        (Let "x" (Val 100) (App (Var "f") (Val 5)))))
  checkInt "fork returns fresh channel" 40 0 forkOnly
  checkInt "fork twice relay chain" 100 13 threeWayRelay
  checkInt "rendezvous" 80 10 choiceRendezvous
  checkInt "bidirectional channel" 120 42 increment
  checkInt "EOPL LETREC sum" 200 10 sumTo
  checkInt "letrec shadows enclosing f" 100 4
    (Let "f" (Val 88) (LetRec "f" "x" (Var "x") (App (Var "f") (Val 4))))
  checkInt "letrec scope restored" 100 23
    (Let "f" (Val 20)
      (Add (LetRec "f" "x" (Var "x") (App (Var "f") (Val 3))) (Var "f")))
  checkInt "let scope restored" 100 25
    (Let "x" (Val 20) (Add (Let "x" (Val 5) (Var "x")) (Var "x")))
  checkInt "nested letrec lexical capture" 160 9
    (Let "outer" (Val 7)
      (LetRec "f" "x" (Add (Var "x") (Var "outer"))
        (Let "outer" (Val 100) (App (Var "f") (Val 2)))))
  assert "invalid type yields no integer" (null (sourceInts 30 stuckType) && null (machineInts 30 stuckType))
  assert "LOOKUP uses identifier" (comp (Var "alpha") HALT == LOOKUP "alpha" HALT)
  assert "LETREC is a dedicated instruction" (case comp sumTo HALT of
    LETREC "sum" "n" _ _ HALT -> True
    _ -> False)
  assert "fork child binder scope" (checkClosed (Fork (Abs "channel" (Receive (Var "channel")))) == Right ())
  assert "fork binder not in parent scope" (case checkClosed
    (Let "z" (Fork (Abs "channel" (Val 1))) (Var "channel")) of
    Left _ -> True
    _ -> False)
  assert "unbound identifier" (case checkClosed (Var "oops") of Left _ -> True; _ -> False)

  -- Every actor example should produce a nonempty before/after translation pair,
  -- and the translated program should remain closed.
  mapM_ (\(name, (before, after)) ->
    assert ("actor translation output for " ++ name)
      (before /= "" && after /= "" && before /= after)) actorTranslationPairs

  -- The translated programs should also satisfy the same closedness and round-trip
  -- invariant as the original actor programs.
  mapM_ (\(name, (src, tgt)) ->
    assert ("actor translation validity for " ++ name)
      (actorToChannel src == tgt && checkClosed tgt == Right ())) actorExampleTranslations

  putStrLn "All bounded tests passed (not a bisimilarity proof)."
