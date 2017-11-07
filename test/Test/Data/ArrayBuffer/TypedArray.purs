module Test.Data.ArrayBuffer.TypedArray where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.ArrayBuffer.TypedArray as TA
import Data.Maybe (Maybe)
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
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffset ab 2 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 2 2 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 2 3 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 3 3 :: Maybe TA.Int8Array)

unsafeArrayToNodeBuffer :: Array NB.Octet -> NB.Buffer
unsafeArrayToNodeBuffer = unsafePerformEff <<< NB.fromArray
