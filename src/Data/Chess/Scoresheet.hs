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

logowithurl ::
  IO (Diagram B R2)
logowithurl =
  do l <- fmap (either text (\l -> image l # sized (Dims 140 77.33))) (loadImageExt "etc/tgcc-logo.png")
     let t = text "http://thegapchessclub.org.au" # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
         r = rect 140 10 # lw none
     return ((t <> r) === l)
     
{-
date
event
round
board#
time control
remove colon
number column thinner
line thinner
remove shading on time box
decrease shading for black
-}

namebox ::
  Diagram B R2
namebox =
  let nametext c = alignedText (-0.1) 0 c # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
      ratingtext = alignedText (-0.1) 0 "rating" # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
      name c s = nametext c <> rect 300 20 # lc darkblue # fc s # alignL
      rating s = ratingtext <> rect 78 20 # lc darkblue # fc s # alignL
      box c s = hcat' (with & sep .~ 20) [name c s, rating s] # centerX
  in box "white" whiteshading === box "black" blackshading
  
row ::
  Int
  -> Diagram B R2
row r =
  let numbertext n = alignedText 1.2 0.5 (show n) # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc white
      number n = numbertext n <> rect 20 20 # alignR # fc darkblue # lc darkblue
      whitemove = rect 60 20 # fc whiteshading # lc darkblue
      blackmove = rect 60 20 # fc blackshading # lc darkblue
      time = rect 30 20 # fc timeboxshading # lc darkblue
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
