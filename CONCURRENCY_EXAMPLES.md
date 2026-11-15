# CTree를 이용한 동시성 표현

이 문서는 `CTree.hs`와 `Channel.hs`만을 사용하여 동시성을 어떻게 표현하는지 설명합니다.

## 동시성 예제 파일

- **ConcurrencyExample.hs**: 5가지 동시성 예제 정의
- **ConcurrencyExampleRun.hs**: 예제 분석 및 실행

## 실행 방법

```bash
cabal run concurrent-letrec -- concurrency-example
```

## CTree의 구성 요소

### 1. CTree 데이터 타입

```haskell
data CTree e a where
  Now    :: a -> CTree e a                          -- 계산 완료
  Choice :: CTree e a -> CTree e a -> CTree e a     -- 비결정적 선택
  Zero   :: CTree e a                               -- 실패/불가능
  Eff    :: e b -> (b -> CTree e a) -> CTree e a    -- 이펙트 수행
  Later  :: CTree e a -> CTree e a                  -- 시간 경과
```

### 2. CTreeC (Codensity Monad)

```haskell
newtype CTreeC e a = CTreeC
  { runCTreeC :: forall r. (a -> CTree e r) -> CTree e r }
```

연속을 이용한 최적화된 모나드 표현

## 핵심 개념

### Later: 시간의 경과

- 동시성에서 "한 번에 한 단계씩" 실행을 표현
- 모든 동시 액션은 최소한 한 단계(Later)를 거쳐야 함
- 무한 루프 방지 (guardedness 보장)

### Choice: 인터리빙 (Interleaving)

- 두 프로세스 중 어느 것이 먼저 실행될지 비결정적으로 선택
- 모든 가능한 실행 순서를 탐색 가능
- `parallel` 함수에서:
  - `left`: 왼쪽 프로세스가 먼저 진행
  - `right`: 오른쪽 프로세스가 먼저 진행
  - `both`: 동기화 가능하면 동기화

### Eff: 이펙트/이벤트

- 프로세스가 수행하는 실제 액션 (송수신, 계산 등)
- `ChanEff` 타입으로 정의:
  - `SendInt`: 채널로 정수 전송
  - `ReceiveInt`: 채널에서 정수 수신
  - `NewChan`: 새 채널 생성

### Synchronization: 동기화

```haskell
instance Concurrent ChanEff where
  interact (SendInt c n) (ReceiveInt d) | c == d = Later (Now ((), n))
  interact (ReceiveInt c) (SendInt d n) | c == d = Later (Now (n, ()))
  interact _ _ = Zero
```

- 같은 채널에서 `SendInt`와 `ReceiveInt`가 만나면 동기화
- 값이 전달되고 시간이 한 단계 진행
- 불일치하면 `Zero` (그 경로는 불가능)

## 예제 1: 간단한 송수신

```haskell
sender1 = send 0 42
receiver1 = receive 0
example1 = parallel sender1 receiver1
```

**CTree 구조**:
```
Choice(
  Left: sender가 먼저 send(42)
  Right: receiver가 먼저 receive (차단됨)
  Both: 동기화 -> Later(Now((), 42))
)
```

**결과**: `((), 42)` - sender는 (), receiver는 42를 얻음

## 예제 2: Request-Response 패턴

```haskell
processA = do
  send 0 10
  resp <- receive 1
  return (10, resp)

processB = do
  val <- receive 0
  send 1 (val * 2)
  return (val, val * 2)

example2 = parallel (lower processA) (lower processB)
```

**동작 흐름**:
1. A가 채널 0으로 10 전송
2. B가 채널 0에서 10 수신
3. B가 채널 1로 20 전송
4. A가 채널 1에서 20 수신

**결과**: `((10, 20), (10, 20))`

## 예제 3: 비결정적 선택

```haskell
flexibleSender = choiceC (send 0 100) (send 1 200)
example3a = parallel flexibleSender (receive 0)
example3b = parallel flexibleSender (receive 1)
```

**특징**:
- `Choice`로 인해 두 가지 실행 경로 생성
- 송신자가 채널 0 또는 1 선택 가능
- 수신자와 일치하는 경로만 동기화

## 예제 4: Later를 이용한 타이밍 제약

```haskell
delayedSender = laterC (send 0 99)
immediateReceiver = receive 0
example4 = parallel delayedSender immediateReceiver
```

**특징**:
- `Later`로 한 단계 대기
- 첫 단계에서 receiver는 차단됨 (Zero)
- 두 번째 단계에서 동기화

**결과**: `Later(Now((), 99))` - 시간이 한 단계 진행됨

## 예제 5: 파이프라인 (3개 프로세스)

```haskell
p1 = send 0 5
p2 = do
  x <- receive 0
  send 1 (x + 10)
p3 = receive 1
```

**데이터 흐름**:
- P1 -> (채널 0) -> P2 -> (채널 1) -> P3
- 5 -> 15

## CTreeC의 역할

### 효율성 개선

일반 CTree의 깊게 중첩된 bind 연산:
```
p1 >>= (\a -> p2 >>= (\b -> p3 >>= (\c -> ...)))
```

각 단계마다 중간 CTree 객체가 생성되어 메모리/시간 오버헤드 발생.

CTreeC는 **연속을 조합**하므로:
```
CTreeC (\k -> runCTreeC p1 (\a -> runCTreeC p2 (\b -> ...)))
```

함수 합성이 CTree 구조 합성보다 훨씬 효율적.

### parRC: 비동기 병렬화

```haskell
parRC :: Concurrent e => CTreeC e a -> CTreeC e b -> CTreeC e b
parRC p q = CTreeC (\k -> parR (lower p) (runCTreeC q k))
```

- `q`의 연속 `k`만 전달 (오른쪽 편향)
- 왼쪽은 `lower`로 일반 CTree로 변환
- 비동기적 동작 표현

## 요약: CTree의 동시성 모델

| 개념 | 표현 방식 |
|------|---------|
| **시간 진행** | `Later` 노드 |
| **인터리빙** | `Choice`와 `parallel` |
| **동기화** | `Concurrent.interact` |
| **비결정성** | `Choice`로 모든 경로 탐색 |
| **실패** | `Zero`로 불가능한 경로 제거 |
| **최적화** | `CTreeC` (연속 기반) |

## 실행 결과 해석

각 예제의 결과는 다음을 의미합니다:

- **Now(v)**: 값 v로 계산 완료
- **Later(tree)**: 한 단계 시간 진행 후 tree 계속
- **Choice(p, q)**: p 또는 q 중 하나의 경로 선택 가능
- **Zero**: 이 경로는 불가능 (예: 송수신 불일치)
- **Eff(...)**: 이펙트 수행 필요

## 참고

이 예제들은 다음 파일을 기반으로 합니다:
- CTree.hs: CTree와 CTreeC의 정의
- Channel.hs: 채널 이펙트와 동기화 규칙
- ConcurrencyExample.hs: 실제 예제 코드
- ConcurrencyExampleRun.hs: 예제 분석 및 설명
