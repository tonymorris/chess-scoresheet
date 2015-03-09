module Main where

import Data.Chess.Scoresheet
import Data.Maybe
import Diagrams.Prelude
import System.IO(IO)

main ::
  IO ()
main =
  renderChessScoresheets (mkSizeSpec (Just 800) Nothing)
