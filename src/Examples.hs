module Examples
  ( examples, increment, choiceRendezvous, sumTo, stuckType, forkOnly, threeWayRelay
  ) where

import Syntax

-- Child: receive c, send (n+1). Parent: send 41, receive 42.
increment :: Expr
increment =
  Let "c" (Fork (Abs "c"
    (Let "n" (Receive (Var "c"))
      (Send (Var "c") (Add (Var "n") (Val 1))))))
    (Let "sent" (Send (Var "c") (Val 41))
      (Receive (Var "c")))

-- EOPL-style recursive environment, no Z combinator.
sumTo :: Expr
sumTo = LetRec "sum" "n"
  (IfZero (Var "n") (Val 0)
    (Add (Var "n")
      (App (Var "sum") (Add (Var "n") (Val (-1))))))
  (App (Var "sum") (Val 4))

choiceRendezvous :: Expr
choiceRendezvous =
  Let "c" (Fork (Abs "c" (Send (Var "c") (Val 10))))
    (Receive (Var "c"))

stuckType :: Expr
stuckType = Add (Abs "x" (Var "x")) (Val 1)

forkOnly :: Expr
forkOnly = Fork (Abs "c" (Val 99))

-- Main spawns A, A spawns B, and the value flows main -> A -> B -> A -> main.
-- This gives a concrete two-fork relay pattern for testing nested concurrency.
threeWayRelay :: Expr
threeWayRelay =
  let bodyB =
        Let "n" (Receive (Var "b"))
          (Send (Var "a") (Add (Var "n") (Val 2)))
      bodyA =
        Let "n" (Receive (Var "a"))
          (Let "_" (Send (Var "b") (Add (Var "n") (Val 1)))
            (Let "m" (Receive (Var "a"))
              (Send (Var "a") (Add (Var "m") (Val 3)))))
  in Let "a"
       (Fork (Abs "a" (Let "b" (Fork (Abs "b" bodyB)) bodyA)))
       (Let "_" (Send (Var "a") (Val 7))
         (Receive (Var "a")))

examples :: [(String, Expr)]
examples =
  [ ("increment",increment)
  , ("letrec-sum",sumTo)
  , ("rendezvous",choiceRendezvous)
  , ("type-error",stuckType)
  , ("fork",forkOnly)
  , ("fork-twice",threeWayRelay)
  ]
