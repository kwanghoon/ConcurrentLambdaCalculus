-- | A single EOPL-style abstract syntax tree with named identifiers.
-- No surface/core split, lexical-address conversion, or fixpoint desugaring.
module Syntax
  ( Identifier, Expr(..), checkClosed ) where

type Identifier = String

-- | Fork binds its channel name only in the child expression.
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
  | Fork Identifier Expr
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
    check xs (Fork ch child) = check (ch:xs) child
