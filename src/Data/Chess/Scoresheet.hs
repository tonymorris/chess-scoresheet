module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Chess.OutputFormat
import Data.Colour.SRGB
import Data.List
import Diagrams.Backend.Cairo.Internal
import Prelude
import System.Directory
import System.FilePath

whiteshading ::
  Colour Double
whiteshading =
  white

blackshading ::
  Colour Double
blackshading =
  let g = 232
  in sRGB24 g g g

timeboxshading ::
  Colour Double
timeboxshading =
  white

maincolour ::
  Colour Double
maincolour =
  darkblue

dejavuSansMono ::
  HasStyle a =>
  a
  -> a

dejavuSansMono =
  font "DejaVu Sans Mono"
logowithurl ::
  IO (Diagram B R2)
logowithurl =
  do l <- fmap (either text (\l -> image l # sized (Dims 140 77.33))) (loadImageExt "etc/tgcc-logo.png")
     let t = text "http://thegapchessclub.org.au" # dejavuSansMono # fontSizeL 5 # fc maincolour
         r = rect 140 10 # lw none
     return ((t <> r) === l)
     
{-
date
event
round
board#
time control
result
remove colon
number column thinner
line thinner
remove shading on time box
decrease shading for black
-}

namebox ::
  Diagram B R2
namebox =
  let nametext c = alignedText (-0.1) 0 c # dejavuSansMono # fontSizeN 0.01 # fc maincolour
      ratingtext = alignedText (-0.1) 0 "rating" # dejavuSansMono # fontSizeN 0.01 # fc maincolour
      name c s = nametext c <> rect 300 20 # lc maincolour # fc s # alignL
      rating s = ratingtext <> rect 78 20 # lc maincolour # fc s # alignL
      box c s = hcat' (with & sep .~ 20) [name c s, rating s] # centerX
  in box "white" whiteshading === box "black" blackshading
  
row ::
  Int
  -> Diagram B R2
row r =
  let numbertext n = alignedText 1.2 0.5 (show n) # dejavuSansMono # fontSizeN 0.01 # fc white
      number n = numbertext n <> rect 20 20 # alignR # fc maincolour # lc maincolour # lwG 0.05
      whitemove = rect 60 20 # fc whiteshading # lc maincolour # lwG 0.05
      blackmove = rect 60 20 # fc blackshading # lc maincolour # lwG 0.05
      time = rect 30 20 # fc timeboxshading # lc maincolour # lwG 0.05
  in number r ||| whitemove ||| time ||| blackmove ||| time # centerX
-- 20 60 30 20 60 30
rownumbers ::
  [Int]
  -> Diagram B R2
rownumbers =
  foldr ((===) . row) mempty

rows1 ::
  Diagram B R2
rows1 =
  (rownumbers [1..25] ||| rownumbers [26..50]) # centerX

scoresheet ::
  Diagram B R2
  -> Diagram B R2
scoresheet l =
   let r = vcat' (with & sep .~ 10) [namebox, rows1]
   in vcat' (with & sep .~ 5) [l # alignL, r # alignL]
  
renderChessScoresheet ::
  OutputFormat
  -> SizeSpec2D
  -> IO ()
renderChessScoresheet t s =
  let outputDirectory = "out"
      options = CairoOptions (outputDirectory </> "chess-scoresheet" <//> t) s (formatType t) False
  in do createDirectoryIfMissing True outputDirectory
        l <- logowithurl
        fst (renderDia Cairo options (scoresheet l))

renderChessScoresheets ::
  SizeSpec2D
  -> IO ()
renderChessScoresheets s =
  mapM_ (`renderChessScoresheet` s) [PDF' ..]
