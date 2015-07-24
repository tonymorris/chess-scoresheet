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

logowithtext ::
  IO (Diagram B)
logowithtext =
  let logowidth = 420
      logoheight = 232
      logoscale = 3.5
      logodims = dims2D (logowidth / logoscale) (logoheight / logoscale)
  in
    do l <- fmap (either text (\l -> image l # sized logodims)) (loadImageExt "etc/tgcc-logo.png")
       let t = text "logo text"
           r = rect 140 10 # lw none
       return ((t <> r) === l)
    
box1 ::
  Diagram B
box1 =
  let textbox c = alignedText (-0.1) 0 c 
      labelbox c w = textbox c <> rect w 40 # alignL
  in hcat' (with & sep .~ 20) [labelbox "papa bear" 290, labelbox "baby bear" 78] # centerX

box2 ::
  Diagram B
box2 =
  let textbox c = alignedText (-0.1) (-0.3) c
      labelbox c w = textbox c <> rect w 40 # lwL 1.2 # alignL      
  in labelbox "mama bear" 242

scoresheet ::
  Diagram B
  -> Diagram B
scoresheet l =
   vcat' (with & sep .~ 5) [(hcat' (with & sep .~ 10) [l, box2]) # alignL, box1 # alignL]
  
scoresheetwithlogo ::
  IO (Diagram B)
scoresheetwithlogo =
  do l <- logowithtext
     return (scoresheet l)

renderChessScoresheet ::
  OutputFormat 
  -> SizeSpec V2 Double
  -> IO ()
renderChessScoresheet t s =
  let outputDirectory = "out"
      options = CairoOptions (outputDirectory </> "chess-scoresheet" <//> t) s (formatType t) False
  in do createDirectoryIfMissing True outputDirectory
        ss <- scoresheetwithlogo
        fst (renderDia Cairo options ss)

renderChessScoresheets ::
  SizeSpec V2 Double
  -> IO ()
renderChessScoresheets s =
  mapM_ (`renderChessScoresheet` s) [PDF' ..]
