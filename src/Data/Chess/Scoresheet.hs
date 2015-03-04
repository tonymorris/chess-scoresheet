module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.SVG
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
  Diagram B R2
row =
  let numbertext n = text (show n) # alignR # font "DejaVu Sans Mono" # fontSizeN 0.15 # fc white -- how to right-align
      number n = numbertext n <> rect 2 2 # fc darkblue # lc (sRGB24 217 217 217) # lc darkblue
      whitemove = rect 6 2 # lc darkblue
      blackmove = rect 6 2 # fc (sRGB24 192 192 192) # lc darkblue
      time = text ":" # fc darkblue <> rect 3 2 # fc (sRGB24 217 217 217) # lc darkblue
  in number (12 :: Int) ||| whitemove ||| time ||| blackmove ||| time

scoresheet ::
  Diagram B R2
scoresheet =
  row
  