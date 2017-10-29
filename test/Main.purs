module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.TypedArray.Int8Array as I8A

main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = do
  let xs = I8A.fromArray [0, 1, 2]
  logShow $ I8A.length xs
  logShow $ I8A.index xs 1
  logShow $ I8A.index xs 4
