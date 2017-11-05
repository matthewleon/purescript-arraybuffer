module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)--, log, logShow)
{-
import Data.ArrayBuffer.TypedArray as TA
import Data.ArrayBuffer.TypedArray.Raw as TAR
-}

import Test.Data.ArrayBuffer.DataView (testDataView)
import Test.Data.ArrayBuffer.TypedArray (testTypedArray)

main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = do
  testDataView
  testTypedArray
