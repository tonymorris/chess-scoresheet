module Main where

import Data.Chess.Scoresheet
import Diagrams.Backend.Cairo.CmdLine
import System.IO(IO)

main ::
  IO ()
main =
  mainWith scoresheet
