module Test.Data.ArrayBuffer.TypedArray where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.ArrayBuffer.TypedArray as TA
import Data.Maybe (Maybe)

testTypedArray :: forall e. Eff ( console :: CONSOLE | e ) Unit
testTypedArray =
  let arr = [100, 101, 102, 103]
      i8a :: TA.Int8Array
      i8a = TA.fromArray arr
      ab  = TA.buffer i8a
  in do
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int8Array)
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int16Array)
    log $ TA.show (TA.fromArrayBuffer ab :: TA.Int32Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffset ab 2 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 2 2 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 2 3 :: Maybe TA.Int8Array)
    logShow $ TA.show <$> (TA.fromArrayBufferWithOffsetAndLength ab 3 3 :: Maybe TA.Int8Array)
