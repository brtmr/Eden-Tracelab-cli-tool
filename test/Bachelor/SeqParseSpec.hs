module Bachelor.SeqParseSpec where

import Test.Hspec
import Bachelor.SeqParse
import Bachelor.Types
import Control.Lens
import Test.QuickCheck
import qualified GHC.RTS.Events as Ev

spec :: Spec
spec = do
    describe "Bachelor.SeqParse" $ do
        context "ProcessState manipulation with setProcessState" $ do
            it "sets an empty Process to Idle" $ do
                setProcessState emptyProcess `shouldBe` emptyProcess
            it "sets a process with a single running Process to running" $ do
                (setProcessState $ addRunningThread $ emptyProcess)^.p_state
                    `shouldBe` Running
        context "ProcessState manipulation with updateThreadCount" $ do
            it "sum of threads is always equal to p_tTotal" $ property $
                sumPropertyProcesses
        context "MachineState manipulation with updateProcessCount" $ do
            it "sum of processes is always equal to m_pTotal" $ property  $ do
                sumPropertyMachines
        --defining tests for handleThreadEvent for each Event is challenging,
        --but there are multiple property-based tests that can ensure that
        --the implementation is sound.
        {-
        context "Event handling" $ do
            it "handling and event creates a valid RTSState." $ do
                undefined
        -}

main :: IO()
main = hspec spec

{-
 - Arbitrary instances.
 - -}
instance Arbitrary RunState where
    arbitrary = oneof [return Running,
                       return Runnable,
                       return Idle,
                       return Blocked]

instance Arbitrary ProcessState where
    arbitrary = do
        runState <- arbitrary
        Positive mid <- arbitrary
        Positive ts  <- arbitrary
        Positive running <- arbitrary
        Positive blocked <- arbitrary
        Positive runnable <- arbitrary
        let total = running+blocked+runnable
        return $ ProcessState {
            _p_parent    = mid,
            _p_state     = runState,
            _p_timestamp = ts,
            _p_tRunning  = running,
            _p_tRunnable = runnable,
            _p_tBlocked  = blocked,
            _p_tTotal    = total
        }

instance Arbitrary MachineState where
    arbitrary = do
        state <- arbitrary
        Positive ts <- arbitrary
        Positive running <- arbitrary
        Positive blocked <- arbitrary
        Positive runnable <- arbitrary
        return $ MachineState {
            _m_state     = state,
            _m_timestamp = ts,
            _m_pRunning  = running,
            _m_pRunnable = runnable,
            _m_pBlocked  = blocked,
            _m_pTotal    = running + runnable + blocked
        }

mkThreadState :: Ev.Timestamp -> Ev.ProcessId -> Gen ThreadState
mkThreadState ts pid = do
    state <- arbitrary
    return $ ThreadState {
        _t_parent      = pid,
        _t_state       = state,
        _t_timestamp   = ts
        }

{-
helpful values
-}
emptyProcess = ProcessState {
    _p_parent    = 1,
    _p_state     = Idle,
    _p_timestamp = 100,
    _p_tRunning  = 0,
    _p_tRunnable = 0,
    _p_tBlocked  = 0,
    _p_tTotal    = 0
    }

addRunningThread p = updateThreadCount p Nothing (Just Running)

{-
 - QuickCheck Properties
 - -}

 {-updating the thread count should always leave
 - the sum p_tTotal equal to the sum of the threads -}
sumPropertyProcesses :: ProcessState -> (Maybe RunState) -> (Maybe RunState) -> Bool
sumPropertyProcesses pstate s1 s2 =
    let e = updateThreadCount pstate s1 s2
    in e^.p_tTotal == (e^.p_tRunning + e^.p_tBlocked + e^.p_tRunnable)

sumPropertyMachines :: MachineState -> (Maybe RunState) -> (Maybe RunState) -> Bool
sumPropertyMachines mState s1 s2 =
    let e = updateProcessCount mState s1 s2
    in _m_pTotal e == (_m_pRunning e + _m_pBlocked e + _m_pRunnable e)
