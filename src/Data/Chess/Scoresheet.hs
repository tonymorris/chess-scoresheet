module Data.Chess.Scoresheet where

import Diagrams.Prelude
import Diagrams.Backend.SVG
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
  let number = rect 2 2
      move = rect 6 2
      time = rect 3 2
  in number ||| move ||| time ||| move ||| time

scoresheet ::
  Diagram B R2
scoresheet =
  row
  