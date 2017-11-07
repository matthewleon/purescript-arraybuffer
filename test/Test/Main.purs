module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Data.ArrayBuffer.DataView (testDataView)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
--import Test.Data.ArrayBuffer.TypedArray (testTypedArray)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  testDataView
