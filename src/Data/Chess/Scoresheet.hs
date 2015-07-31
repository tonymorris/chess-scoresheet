{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- http://projects.haskell.org/diagrams/doc/manual.html
module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Chess.OutputFormat
import Diagrams.Backend.Cairo.Internal
import Prelude hiding (round)
import System.Directory
import System.FilePath

box ::
  Diagram B
box =
  let labelbox c w = text c <> rect w 40 # lwL 1.2 # alignL      
  in labelbox "abcdefghijkl" 242

renderChessScoresheet ::
  OutputFormat 
  -> SizeSpec V2 Double
  -> IO ()
renderChessScoresheet t s =
  let outputDirectory = "out"
      options = CairoOptions (outputDirectory </> "chess-scoresheet" <//> t) s (formatType t) False
  in do createDirectoryIfMissing True outputDirectory
        fst (renderDia Cairo options box)

renderChessScoresheets ::
  SizeSpec V2 Double
  -> IO ()
renderChessScoresheets s =
  mapM_ (`renderChessScoresheet` s) [PDF' ..]
