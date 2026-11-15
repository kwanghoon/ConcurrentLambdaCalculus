{-# LANGUAGE GADTs #-}

-- | 간단한 동시성 예제들 - CTree와 Channel을 이용한 병렬 프로세스 표현
module ConcurrencyExample where

import CTree
import Channel

-- ============================================================================
-- 예제 1: 간단한 송수신
-- ============================================================================
-- 두 프로세스가 채널을 통해 값을 교환하는 가장 간단한 경우

-- 프로세스 P1: 채널 0으로 42를 전송
sender1 :: CTreeC ChanEff ()
sender1 = send 0 42

-- 프로세스 P2: 채널 0에서 값을 수신
receiver1 :: CTreeC ChanEff Int
receiver1 = receive 0

-- 두 프로세스를 병렬로 실행
-- interp와 interpStC를 이용해 채널 이펙트를 처리할 수 있음
example1 :: CTree ChanEff ((), Int)
example1 = parallel (lower sender1) (lower receiver1)


-- ============================================================================
-- 예제 2: 연쇄 통신
-- ============================================================================
-- 프로세스 A: 값을 보내고, 응답을 받음
processA :: CTreeC ChanEff (Int, Int)
processA = do
  send 0 10  -- 채널 0으로 10 전송
  resp <- receive 1  -- 채널 1에서 응답 수신
  return (10, resp)

-- 프로세스 B: 값을 받고, 응답을 보냄
processB :: CTreeC ChanEff (Int, Int)
processB = do
  val <- receive 0  -- 채널 0에서 값 수신
  send 1 (val * 2)  -- 채널 1로 값*2 전송
  return (val, val * 2)

-- 두 프로세스의 병렬 실행
-- A가 10을 보내면, B가 받아서 20을 응답으로 보냄
example2 :: CTree ChanEff ((Int, Int), (Int, Int))
example2 = parallel (lower processA) (lower processB)


-- ============================================================================
-- 예제 3: 비결정적 선택을 포함한 동시성
-- ============================================================================
-- 한 프로세스가 두 개의 채널 중 하나를 선택할 수 있는 경우

-- 송신자: 채널 0 또는 채널 1 중 하나로 전송할 수 있음
flexibleSender :: CTreeC ChanEff ()
flexibleSender = choiceC (send 0 100) (send 1 200)

-- 수신자 1: 채널 0에서 수신
receiver2a :: CTreeC ChanEff Int
receiver2a = receive 0

-- 수신자 2: 채널 1에서 수신
receiver2b :: CTreeC ChanEff Int
receiver2b = receive 1

-- 송신자와 수신자 1의 병렬 실행
-- choiceC에 의해 두 가지 가능한 실행: 
-- 1) sender가 0으로 전송 -> receiver2a가 100 수신
example3a :: CTree ChanEff ((), Int)
example3a = parallel (lower flexibleSender) (lower receiver2a)

-- 또는 송신자와 수신자 2의 병렬 실행
-- 2) sender가 1로 전송 -> receiver2b가 200 수신
example3b :: CTree ChanEff ((), Int)
example3b = parallel (lower flexibleSender) (lower receiver2b)


-- ============================================================================
-- 예제 4: Later를 이용한 지연 표현
-- ============================================================================
-- 타이밍 제약이 있는 병렬 프로세스

-- 지연 후 전송: 한 단계 대기 후 전송
delayedSender :: CTreeC ChanEff ()
delayedSender = laterC (send 0 99)

-- 즉시 수신: 바로 수신 시도
immediateReceiver :: CTreeC ChanEff Int
immediateReceiver = receive 0

-- 지연된 송신자와 수신자의 병렬 실행
-- 송신자의 Later 때문에 먼저 수신자가 차단되고,
-- 다음 시간 단계에서 송신자가 값을 보냄
example4 :: CTree ChanEff ((), Int)
example4 = parallel (lower delayedSender) (lower immediateReceiver)


-- ============================================================================
-- 예제 5: 세 개 이상의 프로세스 (연쇄 파이프라인)
-- ============================================================================
-- P1 -> [채널0] -> P2 -> [채널1] -> P3

-- 프로세스 P1: 초기값 발송
p1 :: CTreeC ChanEff ()
p1 = send 0 5

-- 프로세스 P2: 채널 0에서 받아서 처리 후 채널 1로 전송
p2 :: CTreeC ChanEff ()
p2 = do
  x <- receive 0
  send 1 (x + 10)

-- 프로세스 P3: 채널 1에서 받음
p3 :: CTreeC ChanEff Int
p3 = receive 1

-- 파이프라인: P1과 P2를 먼저 병렬화
pipeline12 :: CTree ChanEff ((), ())
pipeline12 = parallel (lower p1) (lower p2)

-- 그 결과와 P3를 다시 병렬화
-- (실제로는 순차적으로 동작하도록 CTree 구조가 제약함)
example5 :: CTree ChanEff ((((), ()), Int))
example5 = do
  res12 <- parallel (lower p1) (lower p2)
  res3 <- lower p3
  return (res12, res3)


-- ============================================================================
-- 설명: CTree를 이용한 동시성 모델
-- ============================================================================
-- 
-- 1. CTree의 생성자별 의미:
--    - Now a: 계산 완료, 값 a 반환
--    - Later p: 다음 시간 단계에 p 실행 (시간 진행 표현)
--    - Choice p q: p 또는 q 중 하나 선택 (비결정성, 모든 가능한 인터리빙)
--    - Eff op k: 이펙트 op 수행 후 k로 계속
--    - Zero: 불가능한 경로 (송수신 불일치)
--
-- 2. Channel의 Concurrent 인스턴스:
--    - SendInt와 ReceiveInt가 같은 채널에서 만나면 동기화됨
--    - 값이 전달되고 Later로 시간이 한 단계 진행됨
--    - 불일치하면 Zero (실패)
--
-- 3. parallel 함수:
--    - 두 프로세스의 모든 가능한 인터리빙을 표현
--    - left: 왼쪽 프로세스가 먼저 진행
--    - right: 오른쪽 프로세스가 먼저 진행  
--    - both: 동기화 가능하면 동기화
--
-- 4. 실행 흐름:
--    example1에서 sender1과 receiver1이 병렬 실행될 때:
--    - sender1은 채널 0으로 42 전송
--    - receiver1은 채널 0에서 수신
--    - 두 Eff가 interact로 매칭되어 동기화됨
--    - Later(Now((), 42)) 반환
