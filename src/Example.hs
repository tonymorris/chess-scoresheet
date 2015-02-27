module Main where

import Data.Chess.Scoresheet
import Diagrams.Backend.SVG.CmdLine
import System.IO(IO)

main ::
  IO ()
main =
  mainWith example
