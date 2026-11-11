# Concurrent Lambda Calculus

A standalone Haskell/Cabal teaching project that combines **EOPL LETREC's
extended-environment implementation** with the concurrent lambda calculus,
compiler and stack machine in Bahr and Hutton, *Calculating Compilers for
Concurrency* (ICFP 2023), §8 / Fig. 4.

## Key design decisions

1. **One syntax `Expr` only**; there is no `SExpr`, `elaborate`, or de Bruijn
   index translation. Variables are `String` identifiers.
2. Native `Let` and `LetRec` constructors; **no Z combinator**.
3. Source environments use `EmptyEnv`, `ExtendEnv`, and `ExtendEnvRec`. On
   lookup of recursive function `f`, `applyEnv` constructs a closure whose
   saved environment is the `ExtendEnvRec` node itself.
4. Machine environments mirror the source with `EmptyEnvM`, `ExtendEnvM`,
   and `ExtendEnvRecM`, storing the **compiled function body**.
5. For recursion, the VM adds **one instruction `LETREC`**. Its body ends with the existing
   `RET`, using the existing `CLO next oldEnv` stack frame to restore scope.
   **No `RESTORE` instruction** is added.
6. `Let` is treated as call-by-value `App (Abs x body) rhs` on both the source
   and target sides, preserving the paper's `Later` at function application.
7. `Fork (Abs ch child)` is the channel-calculus form in which the fresh
   channel name is bound in the child as a lambda parameter. The parent receives
   the fresh channel as the returned value of `Fork`.
8. `IfZero` (compiled to the `IFZERO` instruction) is an **EOPL teaching
   extension**, not part of paper §8. `LETREC` is the only instruction added
   for recursion.

## Build and run

Install a recent GHC and cabal-install, then:

```sh
cabal build all
cabal test
cabal run concurrent-letrec -- list
cabal run concurrent-letrec -- increment 120
cabal run concurrent-letrec -- letrec-sum 200
cabal run concurrent-letrec -- rendezvous 100
cabal run concurrent-letrec -- type-error 100
cabal run concurrent-letrec -- fork 100
```

The command-line argument is the maximum number of `Later` (τ) transitions
per explored execution path. `Runner.explore` explores nondeterministic
branches in a bounded fashion; it is **not** a bisimulation checker.

## Actor calculus and channel encoding

The project also includes a small actor-style calculus in `src/ActorSyntax.hs`.
The actor syntax models mailbox-style communication with `spawn`, `send`, and
implicit `receive`, and it is translated into the existing channel calculus by
the `Actor2Channel.actorToChannel` function:

- `spawn M` creates a new actor that evaluates `M`
- `self` names the current actor's mailbox
- `ASend target value` translates to `Send (translated target) (translated value)`
- `AReceive` translates to `Receive (Var self)` where `self` is the current actor's mailbox variable

This is the key difference from the channel calculus: in the channel calculus, a
receive is channel-directed (`receive ch`), while in the actor calculus the
current actor implicitly receives from its own mailbox. The translation threads
that mailbox variable through the actor program so `receive` becomes a receive on
that actor's channel.

In other words, `spawn M` is not `spawn c => ...` in this project: the actor is
created with body `M`, and `self` is the actor's address within that body rather
than a separate parameter passed to the spawn expression.

The translation is intentionally shallow: it reuses the same source language's
`Let`, `Abs`, `App`, `IfZero`, and recursion constructs, while mapping the actor
communication primitives onto the existing channel-based communication primitives.

Examples include `increment`, `rendezvous`, `relay`, `echo`, and `ping`.

```sh
cabal run concurrent-letrec -- actor-list
cabal run concurrent-letrec -- actor-translations
cabal run concurrent-letrec -- actor increment 100
cabal run concurrent-letrec -- actor echo 100
```

The actor and channel programs are pretty-printed with dedicated helpers so the
translation can be inspected before and after rewriting.

## Source structure

```
src/
  Syntax.hs         one identifier-based AST, scope checker, pretty printer
  ActorSyntax.hs    actor-style AST and actor pretty printer
  Actor2Channel.hs  actor-to-channel translation plus pretty-before/after output
  CTree.hs          GADT choice trees, codensity, concurrency, state handler
  Channel.hs        SendInt/ReceiveInt/NewChan and rendezvous
  Eval.hs           EOPL extended Env, applyEnv, source interpreter
  Machine.hs        compiler, recursive EnvM, stack VM, channel handler
  Runner.hs         bounded observation and numeric result comparison
  Examples.hs       LETREC sum (1..n), fork, communication and type-error examples
app/Main.hs        run examples and actor translations
test/Spec.hs       interpreter/VM comparison, scoping tests and actor translation tests
```

## What remains unverified

The repository provides executable interpretations of the paper's choice trees,
not an Agda/Rocq proof. Ordinary lazy Haskell does not enforce the paper's
inductive/coinductive guardedness of `Later`, nor the codensity well-formedness
conditions statically. `compareInts` compares bounded **numeric outcome sets**;
it cannot prove strong bisimilarity or full compiler correctness and cannot
compare non-numeric closures. Nondeterministic exhaustive exploration may be
costly. The paper's correctness theorem requires an additional `LetRec` case
and reasoning about `ExtendEnvRec`/`ExtendEnvRecM` correspondence.

## References

- Patrick Bahr and Graham Hutton, *Calculating Compilers for Concurrency*,
  PACMPL 7(ICFP), 2023, https://doi.org/10.1145/3607855
- EOPL lecture implementations: https://github.com/kwanghoon/Lecture_EOPL
