module Test.Data.ArrayBuffer.TypedArray where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.ArrayBuffer.TypedArray as TA
import Node.Buffer as NB

testTypedArray :: forall e. Eff ( console :: CONSOLE | e ) Unit
testTypedArray =
  let arr = [100, 101, 102, 103]
      nb  = unsafeArrayToNodeBuffer arr
      ab  = unsafePerformEff $ NB.toArrayBuffer nb
  in do
    logShow nb
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int8Array)
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int16Array)
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int32Array)

unsafeArrayToNodeBuffer :: Array NB.Octet -> NB.Buffer
unsafeArrayToNodeBuffer = unsafePerformEff <<< NB.fromArray
