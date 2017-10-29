module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)--, log, logShow)
{-
import Data.ArrayBuffer.TypedArray as TA
import Data.ArrayBuffer.TypedArray.Raw as TAR
-}

import Test.Data.ArrayBuffer.DataView (testDataView)

main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = testDataView
{- return to this later
  let xs  =  (TA.fromArray [0, 1, 2] :: TA.Uint8Array)
      sum  = TAR.reduce (\x y _ _ -> x + y) 0
      sum' = TA.foldl (+) 0
  logShow $ TA.length xs
  logShow $ TA.index xs 1
  logShow $ TA.index xs 4
  log $ TA.toString xs
  log <<< TA.toString $ TAR.map (\i v _ -> i * v) xs
  log <<< TA.toString $ TA.map (_ * 2) xs
  logShow $ sum  xs
  logShow $ sum' xs
  logShow $ TAR.every (\x _ _ -> x > 0) xs
  logShow $ TAR.every (\x _ _ -> x >= 0) xs
  logShow $ TA.every (_ > 0) xs
  logShow $ TA.every (_ >= 0) xs
  log <<< TA.toString $ TA.filter (_ > 0) xs
-}
