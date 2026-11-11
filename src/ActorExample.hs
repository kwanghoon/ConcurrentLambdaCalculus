module ActorExample
  ( ActorExpr(..), actorIncrement, actorRendezvous, actorRelay, actorPing, actorEcho
  , actorThreeWayRelay, actorExamples ) where

import ActorSyntax

-- `spawn M` creates an actor that evaluates `M`. The actor body does not take
-- an explicit mailbox parameter; instead it reads from `self` when it needs the
-- actor address of the current mailbox. To keep the communication pattern
-- consistent, the parent first sends its own `self` and then the payload value.
actorIncrement :: ActorExpr
actorIncrement =
  ALet "c" (ASpawn
    (ALet "peer" AReceive
      (ALet "n" AReceive
        (ASend (AVar "peer") (AAdd (AVar "n") (AVal 1))))))
    (ALet "sentPeer" (ASend (AVar "c") ASelf)
      (ALet "sent" (ASend (AVar "c") (AVal 41))
        AReceive))

actorRendezvous :: ActorExpr
actorRendezvous =
  ALet "c" (ASpawn
    (ALet "peer" AReceive
      (ALet "v" AReceive
        (ASend (AVar "peer") (AVar "v")))))
    (ALet "sentPeer" (ASend (AVar "c") ASelf)
      (ALet "sent" (ASend (AVar "c") (AVal 10))
        AReceive))

-- The child receives the sender first, then the payload, then replies.
actorRelay :: ActorExpr
actorRelay =
  ALet "c" (ASpawn
    (ALet "peer" AReceive
      (ALet "n" AReceive
        (ASend (AVar "peer") (AAdd (AVar "n") (AVal 2))))))
    (ALet "sentPeer" (ASend (AVar "c") ASelf)
      (ALet "reply" (ASend (AVar "c") (AVal 3))
        AReceive))

-- The child echoes the payload back to the sender discovered by the first receive.
actorEcho :: ActorExpr
actorEcho =
  ALet "c" (ASpawn
    (ALet "peer" AReceive
      (ALet "n" AReceive
        (ASend (AVar "peer") (AVar "n")))))
    (ALet "sentPeer" (ASend (AVar "c") ASelf)
      (ALet "sent" (ASend (AVar "c") (AVal 7))
        AReceive))

-- Parent sends its mailbox first so the child can identify the sender, then sends
-- the payload value and waits for the reply.
actorPing :: ActorExpr
actorPing =
  ALet "c" (ASpawn
    (ALet "peer" AReceive
      (ALet "v" AReceive
        (ASend (AVar "peer") (AVar "v")))))
    (ALet "sentPeer" (ASend (AVar "c") ASelf)
      (ALet "sent" (ASend (AVar "c") (AVal 11))
        AReceive))

-- Main actor spawns A, A spawns B, and each actor receives the sender and then the
-- payload before replying upstream. Each send sequence first identifies the
-- sender via `self`, then transmits the value.
actorThreeWayRelay :: ActorExpr
actorThreeWayRelay =
  let bodyB =
        ALet "peer" AReceive
          (ALet "n" AReceive
            (ASend (AVar "peer") (AAdd (AVar "n") (AVal 2))))
      bodyA =
        ALet "peer" AReceive
          (ALet "n" AReceive
            (ALet "b" (ASpawn bodyB)
              (ALet "_" (ASend (AVar "b") ASelf)
                (ALet "_" (ASend (AVar "b") (AAdd (AVar "n") (AVal 1)))
                  (ALet "m" AReceive
                    (ASend (AVar "peer") (AAdd (AVar "m") (AVal 3))))))))
  in ALet "a"
       (ASpawn bodyA)
       (ALet "sentPeer" (ASend (AVar "a") ASelf)
         (ALet "_" (ASend (AVar "a") (AVal 7))
           AReceive))

actorExamples :: [(String, ActorExpr)]
actorExamples =
  [ ("increment", actorIncrement)
  , ("rendezvous", actorRendezvous)
  , ("relay", actorRelay)
  , ("echo", actorEcho)
  , ("ping", actorPing)
  , ("three-way-relay", actorThreeWayRelay)
  ]
