module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.SRGB
import Data.List
import Prelude

circ ::
  Diagram B R2
circ =
  circle 1 # fc red # lw none ||| circle 1 # fc green # lw none

----

node ::
  Int
  -> Diagram B R2
node n =
  text (show n) #
  fontSizeN 0.1 #
  fc white <> circle 0.2 #
  fc green #
  named n

arrowOpts ::
  ArrowOpts
arrowOpts =
  with & gaps  .~ small & headLength .~ Global 0.2

tournament ::
  Int
  -> Diagram B R2
tournament n =
  decorateTrail (regPoly n 1) (map node [1..n]) #
  applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]
 
example ::
  Diagram B R2
example =
  tournament 6 #
  connectOutside' (with & gaps .~ small & headLength .~ Global 0.2) (1 :: Int) (2 :: Int)

----

row ::
  Int
  -> Diagram B R2
row r =
  let numbertext n = alignedText 1 0.5 (show n) # font "DejaVu Sans Mono" # fontSizeN 0.04 # fc white
      number n = numbertext n <> rect 20 20 # alignR # fc darkblue # lc (sRGB24 217 217 217)
      whitemove = rect 60 20 # lc darkblue
      blackmove = rect 60 20 # fc (sRGB24 192 192 192) # lc darkblue
      time = text ":" # fontSizeN 0.03 # fc darkblue <> rect 30 20 # fc (sRGB24 217 217 217) # lc darkblue
  in number r ||| whitemove ||| time ||| blackmove ||| time

rowsLeft ::
  Diagram B R2
rowsLeft =
  foldr (\a d -> row a === d) mempty [1..15]

scoresheet ::
  Diagram B R2
scoresheet =
  rowsLeft
  