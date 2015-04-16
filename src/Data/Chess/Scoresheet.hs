-- http://projects.haskell.org/diagrams/doc/manual.html
module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Chess.OutputFormat
import Data.Colour.SRGB
import Data.List
import Diagrams.Backend.Cairo.Internal
import Prelude hiding (round)
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
  let logowidth = 420
      logoheight = 232
      logoscale = 3.5
      logodims = Dims (logowidth / logoscale) (logoheight / logoscale)
  in
    do l <- fmap (either text (\l -> image l # sized logodims)) (loadImageExt "etc/tgcc-logo.png")
       let t = text "http://thegapchessclub.org.au" # dejavuSansMono # fontSizeL 5 # fc maincolour
           r = rect 140 10 # lw none
       return ((t <> r) === l)
     
{-

remove colon
number column thinner
line thinner
remove shading on time box
decrease shading for black
-}

namebox ::
  Diagram B R2
namebox =
  let nametext c = alignedText (-0.1) 0 c # dejavuSansMono # fontSizeL 5 # fc maincolour
      ratingtext = alignedText (-0.1) 0 "rating" # dejavuSansMono # fontSizeL 5 # fc maincolour
      name c s = nametext c <> rect 290 20 # lc maincolour # fc s # lwL 1.2 # alignL
      rating s = ratingtext <> rect 78 20 # lc maincolour # fc s # lwL 1.2 # alignL
      box c s = hcat' (with & sep .~ 20) [name c s, rating s] # centerX
  in box "white" whiteshading === box "black" blackshading
  
row ::
  Int
  -> Diagram B R2
row r =
  let numbertext n = alignedText 1.2 0.5 (show n) # dejavuSansMono # fontSizeL 5 # fc white
      number n = numbertext n <> rect 18 20 # alignR # fc maincolour # lc maincolour # lwL 0.2
      whitemove = rect 60 20 # fc whiteshading # lc maincolour # lwL 0.2
      blackmove = rect 60 20 # fc blackshading # lc maincolour # lwL 0.2
      time = rect 30 20 # fc timeboxshading # lc maincolour # lwL 0.2
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
      textbox c = alignedText (-0.1) (-0.3) c # dejavuSansMono # fontSizeL 4 # fc maincolour
      labelbox c w = textbox c <> rect w 20 # lc maincolour # fc white # lwL 1.2 # alignL      
      event = labelbox "event" 171
      date  = labelbox "date" 71
      round  = labelbox "round" 50
      board  = labelbox "board" 50
      time  = labelbox "time" 71
      result  = labelbox "result" 71
      box = (event ||| date) === (round ||| board ||| time ||| result)
  in do createDirectoryIfMissing True outputDirectory
        l <- logowithurl
        fst (renderDia Cairo options (scoresheet (hcat' (with & sep .~ 10) [l, box])))

renderChessScoresheets ::
  SizeSpec2D
  -> IO ()
renderChessScoresheets s =
  mapM_ (`renderChessScoresheet` s) [PDF' ..]

{-
data Params =
  Params
    (Colour Double) -- whiteshading
    (Colour Double) -- blackshading
    (Colour Double) -- timebox shading
    (Colour Double) -- main colour
    String -- main font

testParams ::
  Params
testParams =
  let g = 232
  in Params 
       white
       (sRGB24 g g g)
       white
       darkblue
      "DejaVu Sans Mono"
-}
