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

logowithurl ::
  IO (Diagram B)
logowithurl =
  let logowidth = 420
      logoheight = 232
      logoscale = 3.5
      logodims = dims2D (logowidth / logoscale) (logoheight / logoscale)
  in
    do l <- fmap (either text (\l -> image l # sized logodims)) (loadImageExt "etc/tgcc-logo.png")
       let t = text "http://thegapchessclub.org.au"
           r = rect 140 10 # lw none
       return ((t <> r) === l)
    
namebox ::
  Diagram B
namebox =
  let textbox c = alignedText (-0.1) 0 c 
      labelbox c w = textbox c <> rect w 40 # lwL 1.2 # alignL
      box c = hcat' (with & sep .~ 20) [labelbox c 290, labelbox "baby bear" 78] # centerX
  in box "papa bear" 

scoresheet ::
  Diagram B
  -> Diagram B
scoresheet l =
   vcat' (with & sep .~ 5) [l # alignL, namebox # alignL]
  
detailbox ::
  Diagram B
detailbox =
  let textbox c = alignedText (-0.1) (-0.3) c
      labelbox c w = textbox c <> rect w 40 # fc white # lwL 1.2 # alignL      
  in labelbox "mama bear" 242

renderChessScoresheet ::
  OutputFormat 
  -> SizeSpec V2 Double
  -> IO ()
renderChessScoresheet t s =
  let outputDirectory = "out"
      options = CairoOptions (outputDirectory </> "chess-scoresheet" <//> t) s (formatType t) False
  in do createDirectoryIfMissing True outputDirectory
        l <- logowithurl
        fst (renderDia Cairo options (scoresheet (hcat' (with & sep .~ 10) [l, detailbox])))

renderChessScoresheets ::
  SizeSpec V2 Double
  -> IO ()
renderChessScoresheets s =
  mapM_ (`renderChessScoresheet` s) [PDF' ..]
