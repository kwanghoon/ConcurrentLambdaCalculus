{-# LANGUAGE GADTs #-}

-- | ConcurrencyExample의 예제들을 실행하고 결과를 출력하는 모듈
module ConcurrencyExampleRun where

import CTree
import Channel

-- ============================================================================
-- 예제들을 CTree 구조로 출력하기 위한 함수들
-- ============================================================================

-- CTree를 문자열로 변환 (간단한 버전)
showCTree :: Show a => CTree ChanEff a -> String
showCTree (Now v) = "Now(" ++ show v ++ ")"
showCTree (Choice p q) = "Choice(" ++ showCTree p ++ ", " ++ showCTree q ++ ")"
showCTree Zero = "Zero"
showCTree (Later p) = "Later(" ++ showCTree p ++ ")"
showCTree (Eff (SendInt c n) _) = "Eff(SendInt " ++ show c ++ " " ++ show n ++ ", ...)"
showCTree (Eff (ReceiveInt c) _) = "Eff(ReceiveInt " ++ show c ++ ", ...)"
showCTree (Eff NewChan _) = "Eff(NewChan, ...)"

-- ============================================================================
-- 각 예제의 구조를 보기 좋게 출력
-- ============================================================================

printExample1 :: IO ()
printExample1 = do
  putStrLn "\n=== Example 1: Simple send-receive ==="
  putStrLn "Code:"
  putStrLn "  sender1 = send 0 42"
  putStrLn "  receiver1 = receive 0"
  putStrLn "  example1 = parallel sender1 receiver1"
  putStrLn "\nExplanation:"
  putStrLn "  - sender1: Send 42 on channel 0"
  putStrLn "  - receiver1: Receive from channel 0"
  putStrLn "  - parallel: Express all possible interleavings"
  putStrLn "\nCTree structure (partial):"
  putStrLn "  Choice("
  putStrLn "    Left: Eff(SendInt 0 42, ...)"
  putStrLn "    Right: Eff(ReceiveInt 0, ...)"
  putStrLn "    Both(sync): Later(Now((), 42))  -- SYNCHRONIZED!"
  putStrLn "  )"
  putStrLn "\nExpected result:"
  putStrLn "  ((), 42)  -- sender gets (), receiver gets 42"

printExample2 :: IO ()
printExample2 = do
  putStrLn "\n=== Example 2: Request-Response pattern ==="
  putStrLn "Code:"
  putStrLn "  processA = do"
  putStrLn "    send 0 10"
  putStrLn "    resp <- receive 1"
  putStrLn "    return (10, resp)"
  putStrLn "  processB = do"
  putStrLn "    val <- receive 0"
  putStrLn "    send 1 (val * 2)"
  putStrLn "    return (val, val * 2)"
  putStrLn "\nExplanation:"
  putStrLn "  - A sends 10 -> B receives it"
  putStrLn "  - B sends 10*2=20 -> A receives it"
  putStrLn "  - Two channels synchronized sequentially"
  putStrLn "\nExpected result:"
  putStrLn "  ((10, 20), (10, 20))  -- Both see the same values"

printExample3 :: IO ()
printExample3 = do
  putStrLn "\n=== Example 3: Nondeterministic choice with concurrency ==="
  putStrLn "Code:"
  putStrLn "  flexibleSender = choiceC (send 0 100) (send 1 200)"
  putStrLn "  example3a = parallel flexibleSender (receive 0)"
  putStrLn "  example3b = parallel flexibleSender (receive 1)"
  putStrLn "\nExplanation:"
  putStrLn "  - Choice creates two execution paths"
  putStrLn "  - Sender can choose channel 0 or 1"
  putStrLn "  - Receiver waits on specific channel"
  putStrLn "  - Only matching paths synchronize"
  putStrLn "\nExpected results:"
  putStrLn "  example3a: ((), 100)  -- sender chooses channel 0"
  putStrLn "  example3b: ((), 200)  -- sender chooses channel 1"

printExample4 :: IO ()
printExample4 = do
  putStrLn "\n=== Example 4: Timing constraints with Later ==="
  putStrLn "Code:"
  putStrLn "  delayedSender = laterC (send 0 99)"
  putStrLn "  immediateReceiver = receive 0"
  putStrLn "  example4 = parallel delayedSender immediateReceiver"
  putStrLn "\nExplanation:"
  putStrLn "  - Later: Wait one time step"
  putStrLn "  - immediateReceiver blocks at first step (Zero)"
  putStrLn "  - Sender sends at second step"
  putStrLn "  - Time progression represented by Later nodes"
  putStrLn "\nExpected result:"
  putStrLn "  Later(Now((), 99))  -- Synchronize after one delay"

printExample5 :: IO ()
printExample5 = do
  putStrLn "\n=== Example 5: Pipeline with three processes ==="
  putStrLn "Code:"
  putStrLn "  p1 = send 0 5          -- Send 5"
  putStrLn "  p2 = receive 0 >>= \\x -> send 1 (x+10)  -- Process and forward"
  putStrLn "  p3 = receive 1          -- Receive result"
  putStrLn "  example5 = parallel p1 p2 >>= ... parallel result p3"
  putStrLn "\nExplanation:"
  putStrLn "  - Multiple parallel calls needed for 3+ processes"
  putStrLn "  - Channel 0: P1 -> P2 communication"
  putStrLn "  - Channel 1: P2 -> P3 communication"
  putStrLn "  - Each step adds Later for time progression"
  putStrLn "\nExpected result:"
  putStrLn "  ((((), ()), 15))  -- P1=5, P2=5->15, P3=15"

-- Main function
main :: IO ()
main = do
  putStrLn "\n=========================================="
  putStrLn "   CTree Concurrency Examples Analysis"
  putStrLn "==========================================" 
  
  printExample1
  printExample2
  printExample3
  printExample4
  printExample5
  
  putStrLn "\n=========================================="
  putStrLn "                 Key Concepts"
  putStrLn "==========================================" 
  
  putStrLn "\n1. CTree Time Model:"
  putStrLn "   - Later: Progress one time step"
  putStrLn "   - All concurrent actions take at least one step"
  putStrLn "   - Prevents infinite loops (guardedness guarantee)"
  
  putStrLn "\n2. Synchronization:"
  putStrLn "   - SendInt and ReceiveInt on same channel synchronize"
  putStrLn "   - Concurrent.interact determines effect matching"
  putStrLn "   - Mismatches result in Zero (impossible path)"
  
  putStrLn "\n3. Nondeterminism:"
  putStrLn "   - Choice: Explore all possible interleavings"
  putStrLn "   - parallel's left/right/both considers all cases"
  putStrLn "   - Entire CTree represents set of all executions"
  
  putStrLn "\n4. Monad Structure:"
  putStrLn "   - >>= for sequential composition"
  putStrLn "   - parallel for parallel composition"
  putStrLn "   - Combination expresses complex protocols"
  
  putStrLn "\n5. CTreeC Role:"
  putStrLn "   - Optimizes deep bind nesting"
  putStrLn "   - parRC provides asynchronous parallelism"
  putStrLn "   - Only combines continuations until lower() called"
  
  putStrLn "\n"
