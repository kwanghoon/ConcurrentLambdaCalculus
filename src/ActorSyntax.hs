module ActorSyntax
  ( Identifier, ActorExpr(..), checkClosedActor, prettyActorExpr ) where

type Identifier = String

-- | A tiny actor-syntax layer over the channel calculus.
-- Spawn binds an actor mailbox name in the child body, send targets an actor,
-- and receive reads from the current actor's mailbox without naming the channel.
data ActorExpr
  = AVal Int
  | AAdd ActorExpr ActorExpr
  | AVar Identifier
  | ASelf
  | AAbs Identifier ActorExpr
  | AApp ActorExpr ActorExpr
  | ALet Identifier ActorExpr ActorExpr
  | ALetRec Identifier Identifier ActorExpr ActorExpr
  | AIfZero ActorExpr ActorExpr ActorExpr
  | ASpawn ActorExpr
  | ASend ActorExpr ActorExpr
  | AReceive
  deriving (Eq, Show)

checkClosedActor :: ActorExpr -> Either String ()
checkClosedActor = check []
  where
    check :: [Identifier] -> ActorExpr -> Either String ()
    check _ (AVal _) = Right ()
    check xs (AVar x)
      | x `elem` xs = Right ()
      | x == "self" = Right ()
      | otherwise = Left ("unbound variable: " ++ x)
    check _ ASelf = Right ()
    check xs (AAdd a b) = check xs a >> check xs b
    check xs (AAbs x body) = check (x:xs) body
    check xs (AApp a b) = check xs a >> check xs b
    check xs (ALet x rhs body) = check xs rhs >> check (x:xs) body
    check xs (ALetRec f x fbody body) =
      check (x:f:xs) fbody >> check (f:xs) body
    check xs (AIfZero tst th el) = check xs tst >> check xs th >> check xs el
    check xs (ASpawn body) = check xs body
    check xs (ASend ch v) = check xs ch >> check xs v
    check _ AReceive = Right ()

prettyActorExpr :: ActorExpr -> String
prettyActorExpr (AVal n) = show n
prettyActorExpr (AAdd a b) = prettyActorExpr a ++ " + " ++ prettyActorExpr b
prettyActorExpr (AVar x) = x
prettyActorExpr ASelf = "self"
prettyActorExpr (AAbs x body) = "λ" ++ x ++ "." ++ prettyActorExpr body
prettyActorExpr (AApp f x) = prettyActorExpr f ++ " " ++ prettyActorExpr x
prettyActorExpr (ALet x rhs body) =
  "let " ++ x ++ " = " ++ prettyActorExpr rhs ++ " in " ++ prettyActorExpr body
prettyActorExpr (ALetRec f x fbody body) =
  "letrec " ++ f ++ " " ++ x ++ " = " ++ prettyActorExpr fbody ++ " in " ++ prettyActorExpr body
prettyActorExpr (AIfZero tst th el) =
  "if0 " ++ prettyActorExpr tst ++ " then " ++ prettyActorExpr th ++ " else " ++ prettyActorExpr el
prettyActorExpr (ASpawn body) = "spawn (" ++ prettyActorExpr body ++ ")"
prettyActorExpr (ASend ch v) = "send " ++ prettyActorExpr ch ++ " " ++ prettyActorExpr v
prettyActorExpr AReceive = "receive"
