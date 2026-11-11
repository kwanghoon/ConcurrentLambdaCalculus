module Actor2Channel
  ( ActorExpr(..), actorToChannel, actorProgramPair, actorTranslationSummary
  , actorTranslationPair, actorTranslationPairs
  , prettyActorProgram, prettyChannelProgram
  , evaluateActor, actorExamples, actorExampleTranslations
  , actorIncrement, actorRendezvous, actorRelay, actorPing, actorEcho ) where

import ActorExample (actorEcho, actorExamples, actorIncrement, actorPing, actorRelay, actorRendezvous)
import ActorSyntax
import CTree (CTreeC, NoEff)
import Eval (Value, evaluate)
import Syntax (Expr(..), prettyExpr)

-- | The actor calculus differs from the channel calculus in one crucial way:
-- a receive does not name a channel. It consumes from the current actor's own
-- mailbox, represented explicitly by `self`, while send targets an actor/mailbox
-- identifier.
--
-- The translation rewrites the implicit actor mailbox to the explicit channel
-- calculus mailbox variable. For example, `receive` becomes `Receive (Var self)`
-- and `self` itself becomes the mailbox variable bound in the actor's child body.
actorToChannel :: ActorExpr -> Expr
actorToChannel expr =
  Let "self" NewChannel (actorToChannelWith "self" expr)

actorToChannelWith :: Identifier -> ActorExpr -> Expr
actorToChannelWith _ (AVal n) = Val n
actorToChannelWith self (AAdd x y) =
  Add (actorToChannelWith self x) (actorToChannelWith self y)
actorToChannelWith _ (AVar x) = Var x
actorToChannelWith self ASelf = Var self
actorToChannelWith self (AAbs x body) =
  Abs x (actorToChannelWith self body)
actorToChannelWith self (AApp f x) =
  App (actorToChannelWith self f) (actorToChannelWith self x)
actorToChannelWith self (ALet x rhs body) =
  Let x (actorToChannelWith self rhs) 
    (actorToChannelWith self body)
actorToChannelWith self (ALetRec f x fbody body) =
  LetRec f x (actorToChannelWith self fbody) 
    (actorToChannelWith self body)
actorToChannelWith self (AIfZero tst th el) =
  IfZero (actorToChannelWith self tst) 
    (actorToChannelWith self th) 
    (actorToChannelWith self el)
actorToChannelWith _ (ASpawn (AAbs x body)) =
  Fork (Abs x (actorToChannelWith x body))
actorToChannelWith _ (ASpawn body) =
  let fresh = "self"
  in Fork (Abs fresh (actorToChannelWith fresh body))
actorToChannelWith self (ASend ch v) =
  Send (actorToChannelWith self ch) (actorToChannelWith self v)
actorToChannelWith self AReceive = Receive (Var self)

-- | A paired view of the actor program and its channel encoding.
actorProgramPair :: ActorExpr -> (ActorExpr, Expr)
actorProgramPair expr = (expr, actorToChannel expr)

prettyActorProgram :: ActorExpr -> String
prettyActorProgram = prettyActorExpr

prettyChannelProgram :: Expr -> String
prettyChannelProgram = prettyExpr

-- | The actor program before translation and the channel program after it.
actorTranslationPair :: ActorExpr -> (String, String)
actorTranslationPair expr = (prettyActorProgram expr, prettyChannelProgram (actorToChannel expr))

actorTranslationPairs :: [(String, (String, String))]
actorTranslationPairs =
  map (\(name, expr) -> (name, actorTranslationPair expr)) actorExamples

actorTranslationSummary :: ActorExpr -> String
actorTranslationSummary expr =
  let (src, tgt) = actorProgramPair expr
  in "Actor: " ++ prettyActorProgram src ++ "\nChannel: " ++ prettyChannelProgram tgt

-- | Evaluate the actor calculus by compiling it to the channel calculus then
-- running the existing interpreter.
evaluateActor :: ActorExpr -> CTreeC NoEff Value
evaluateActor = evaluate . actorToChannel

actorExampleTranslations :: [(String, (ActorExpr, Expr))]
actorExampleTranslations = map (
  \(name, expr) -> (name, actorProgramPair expr)) actorExamples
