-- | A single EOPL-style abstract syntax tree with named identifiers.
-- No surface/core split, lexical-address conversion, or fixpoint desugaring.
module Syntax
  ( Identifier, Expr(..), checkClosed, prettyExpr ) where

type Identifier = String

-- | Fork creates a fresh channel and passes it to the child process as a
-- channel-parameter. In the paper, this often appears as fork c => P, i.e. a
-- lambda-like abstraction over the fresh channel value.
-- IfZero is an explicitly documented EOPL teaching extension to paper §8.
data Expr
  = Val Int
  | Add Expr Expr
  | Var Identifier
  | Abs Identifier Expr
  | App Expr Expr
  | Let Identifier Expr Expr
  | LetRec Identifier Identifier Expr Expr
  | IfZero Expr Expr Expr
  | Send Expr Expr
  | Receive Expr
  | NewChannel
  | Fork Expr
  deriving (Eq, Show)

-- | Optional lexical-scope check. Syntax remains unchanged.
checkClosed :: Expr -> Either String ()
checkClosed = check []
  where
    check :: [Identifier] -> Expr -> Either String ()
    check _ (Val _) = Right ()
    check xs (Var x)
      | x `elem` xs = Right ()
      | otherwise = Left ("unbound variable: " ++ x)
    check xs (Add a b) = check xs a >> check xs b
    check xs (Abs x body) = check (x:xs) body
    check xs (App a b) = check xs a >> check xs b
    check xs (Let x rhs body) = check xs rhs >> check (x:xs) body
    check xs (LetRec f x fbody body) =
      check (x:f:xs) fbody >> check (f:xs) body
    check xs (IfZero tst th el) = check xs tst >> check xs th >> check xs el
    check xs (Send ch v) = check xs ch >> check xs v
    check xs (Receive ch) = check xs ch
    check _ NewChannel = Right ()
    check xs (Fork e) = check xs e

prettyExpr :: Expr -> String
prettyExpr (Val n) = show n
prettyExpr (Add a b) = prettyExpr a ++ " + " ++ prettyExpr b
prettyExpr (Var x) = x
prettyExpr (Abs x body) = "λ" ++ x ++ "." ++ prettyExpr body
prettyExpr (App f x) = prettyExpr f ++ " " ++ prettyExpr x
prettyExpr (Let x rhs body) =
  "let " ++ x ++ " = " ++ prettyExpr rhs ++ " in " ++ prettyExpr body
prettyExpr (LetRec f x fbody body) =
  "letrec " ++ f ++ " " ++ x ++ " = " ++ prettyExpr fbody ++ " in " ++ prettyExpr body
prettyExpr (IfZero tst th el) =
  "if0 " ++ prettyExpr tst ++ " then " ++ prettyExpr th ++ " else " ++ prettyExpr el
prettyExpr (Send ch v) = "send " ++ prettyExpr ch ++ " " ++ prettyExpr v
prettyExpr (Receive ch) = "receive " ++ prettyExpr ch
prettyExpr NewChannel = "newChannel"
prettyExpr (Fork (Abs x body)) = "fork (" ++ x ++ " => " ++ prettyExpr body ++ ")"
prettyExpr (Fork e) = "fork (" ++ prettyExpr e ++ ")"
