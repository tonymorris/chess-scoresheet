module Main where

import Data.Chess.Scoresheet
import Data.Chess.OutputFormat
import Data.Maybe
import Diagrams.Prelude
import System.IO(IO)

main ::
  IO ()
main =
  renderChessScoresheet PDF' (mkSizeSpec (Just 800) Nothing)
