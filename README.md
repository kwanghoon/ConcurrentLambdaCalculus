# Concurrent Lambda Calculus

A standalone Haskell/Cabal teaching project based on Bahr and Hutton,
*Calculating Compilers for Concurrency* (ICFP 2023), §8 / Fig. 4. It builds
everything on one semantic foundation — choice trees (`CTree`) and a channel
effect (`Channel`) — and shows how that foundation supports:

1. **Concurrent examples** written directly with `CTree` and `Channel`,
   independent of any calculus.
2. A **channel-based calculus** with a `CTree`/`Channel`-based evaluator
   (`Eval`), a compiler, and a stack VM (`Machine`).
3. An **actor-based calculus** with a `CTree`/`Channel`-based evaluator
   (`EvalActor`) and a translator into the channel-based calculus
   (`Actor2Channel`).

## Build and run

Install a recent GHC and cabal-install, then:

```sh
cabal build all
cabal test
```

### Concurrent examples (`CTree` + `Channel` only)

```sh
cabal run concurrent-letrec -- concurrency-example
```

Five examples in `src/ConcurrencyExample.hs` — simple send/receive, chained
communication, nondeterministic choice, delay with `Later`, and a
multi-process pipeline — are built only from `CTree` and `Channel`, with no
lambda calculus involved. `src/ConcurrencyExampleRun.hs` prints each tree and
its outcomes. See [CONCURRENCY_EXAMPLES.md](CONCURRENCY_EXAMPLES.md) for a
walkthrough (in Korean).

### Channel-based calculus

```sh
cabal run concurrent-letrec -- list
cabal run concurrent-letrec -- increment 120
cabal run concurrent-letrec -- letrec-sum 200
cabal run concurrent-letrec -- rendezvous 100
cabal run concurrent-letrec -- type-error 100
cabal run concurrent-letrec -- fork 100
```

For each example this prints the expression, the compiled VM code, the source
(interpreter) outcomes, the VM outcomes, and a bounded numeric agreement check.
The optional argument is the maximum number of `Later` (τ) transitions per
explored path. `Runner.explore` explores nondeterministic branches in a bounded
fashion; it is **not** a bisimulation checker.

### Actor-based calculus

```sh
cabal run concurrent-letrec -- actor-list
cabal run concurrent-letrec -- actor-translations
cabal run concurrent-letrec -- actor increment 100
cabal run concurrent-letrec -- actor-eval increment 100
```

Examples: `increment`, `rendezvous`, `relay`, `echo`, `ping`,
`three-way-relay`, `two-clients`.

`two-clients` spawns client1 and client2. The server sends its address to both,
receives 1 from client1, then signals client2 to send 2. Its result is 4
(`1 + 1 + 2`), making the message order observable.

- `actor NAME [FUEL]` translates the actor program to the channel calculus,
  prints both forms and the compiled code, and runs the translated program
  with the channel evaluator.
- `actor-eval NAME [FUEL]` evaluates the actor program directly with
  `EvalActor` and lists all outcomes and the non-stuck ones.
- `actor-translations` prints each actor program next to its channel encoding.

`./run-actors.sh` runs both `actor` and `actor-eval` for every program in
`actor-list`.

## Channel-based calculus: Eval, Compiler, VM

`Syntax.Expr` is the channel calculus: numbers, addition, variables,
abstraction/application, `Let`, `LetRec`, `IfZero`, and the concurrency
primitives `Fork`, `Send`, `Receive` (channel-directed, `receive ch`) and
`NewChannel`.

- `Eval` is the source interpreter into `CTreeC ChanEff Value`, using the
  extended environment `Env` and `applyEnv`; running it under
  `interpStC … handleChannel` yields a `CTree NoEff`.
- `Machine` contains the compiler (`compile`), the stack VM (`exec`/`execute`)
  over `Code`, `EnvM`, and `Stack`, and the channel handler.
- `Runner` observes both sides with bounded exploration and compares numeric
  outcome sets (`compareInts`).

## Actor-based calculus: EvalActor and translation

`ActorSyntax.ActorExpr` models mailbox-style communication with `ASpawn`,
`ASend`, `ASelf`, and an implicit `AReceive`: instead of `receive ch`, the
current actor receives from its own mailbox. `spawn M` is not `spawn c => …`;
the child body `M` refers to its mailbox through `self`.

- `EvalActor` evaluates actor programs directly into `CTreeC ChanEff Value`
  using `CTree` and `Channel`; `self` is threaded through evaluation, and
  `spawn` creates a fresh channel and runs the child in parallel (`parRC`).
  `evalActor` is defined by pattern matching on `ActorExpr`.
- `Actor2Channel.actorToChannel` translates an actor program into the channel
  calculus: it wraps the program in `let self = newChannel in …`, `spawn M`
  becomes `fork (self => M')`, and `receive` becomes `receive self`. Other
  constructs map to their channel-calculus counterparts. The result can be
  pretty-printed (`prettyActorProgram`, `prettyChannelProgram`) and run with the
  channel evaluator and VM.

## Source structure

```
src/
  CTree.hs               GADT choice trees, codensity, parallel, state handler
  Channel.hs             SendInt/ReceiveInt/NewChan effects and rendezvous
  ConcurrencyExample.hs  examples using only CTree and Channel
  ConcurrencyExampleRun.hs  prints and runs those examples
  Syntax.hs              channel-calculus AST, scope checker, pretty printer
  Eval.hs                channel-calculus interpreter (CTree + Channel)
  Machine.hs             compiler, stack VM, channel handler
  Runner.hs              bounded observation and numeric result comparison
  Examples.hs            channel-calculus examples (LETREC sum, fork, ...)
  ActorSyntax.hs         actor-calculus AST and pretty printer
  ActorExample.hs        actor examples
  EvalActor.hs           actor-calculus evaluator (CTree + Channel)
  Actor2Channel.hs       actor-to-channel translation and pretty printing
app/Main.hs              command-line driver
run-actors.sh            run actor and actor-eval for every actor example
test/
  Spec.hs                interpreter/VM comparison, scoping, translation tests
  EvalActorSpec.hs       EvalActor exploration demo (not in the cabal test suite)
  EvalActorTest.hs       EvalActor checks (not in the cabal test suite)
```


## References

- Patrick Bahr and Graham Hutton, *Calculating Compilers for Concurrency*,
  PACMPL 7(ICFP), 2023, https://doi.org/10.1145/3607855
