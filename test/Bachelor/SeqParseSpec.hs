module Bachelor.SeqParseSpec where

import Test.Hspec
import Bachelor.SeqParse
import Bachelor.Types
import Control.Lens
import Test.QuickCheck

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
            it "sum of processes is always equal to p_tTotal" $ property $
                sumProperty

main :: IO()
main = hspec spec

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

emptyProcess = ProcessState {
    _p_parent    = 1,
    _p_state     = Idle,
    _p_timestamp = 100,
    _p_tRunning  = 0,
    _p_tRunnable = 0,
    _p_tBlocked  = 0,
    _p_tTotal    = 0
    }

addRunningThread :: ProcessState -> ProcessState
addRunningThread p = updateThreadCount p Nothing (Just Running)

{- QuickCheck Property: updating the thread count should always leave
 - the sum p_tTotal equal to the sum of the threads -}
sumProperty pstate s1 s2 =
    let e = updateThreadCount pstate s1 s2
    in e^.p_tTotal == (e^.p_tRunning + e^.p_tBlocked + e^.p_tRunnable)

