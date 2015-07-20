module Main where

import Data.Chess.Scoresheet
-- import Data.Test
import Data.Maybe
import Diagrams.Prelude
import System.IO(IO)

main ::
  IO ()
main =
  renderChessScoresheets (mkSizeSpec (V2 (Just 800) Nothing))
