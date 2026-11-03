module Examples
  ( examples, increment, choiceRendezvous, sumTo, stuckType, forkOnly
  ) where

import Syntax

-- Child: receive c, send (n+1). Parent: send 41, receive 42.
increment :: Expr
increment =
  Let "c" (Fork "c"
    (Let "n" (Receive (Var "c"))
      (Send (Var "c") (Add (Var "n") (Val 1)))))
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
  Let "c" (Fork "c" (Send (Var "c") (Val 10)))
    (Receive (Var "c"))

stuckType :: Expr
stuckType = Add (Abs "x" (Var "x")) (Val 1)

forkOnly :: Expr
forkOnly = Fork "c" (Val 99)

examples :: [(String, Expr)]
examples =
  [ ("increment",increment)
  , ("letrec-sum",sumTo)
  , ("rendezvous",choiceRendezvous)
  , ("type-error",stuckType)
  , ("fork",forkOnly)
  ]
