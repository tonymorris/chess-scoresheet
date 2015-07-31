{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Prelude hiding (round)

box ::
  Diagram B
box =
  let labelbox c w = text c <> rect w 40 # lwL 1.2 # alignL      
  in labelbox "abcdefghijkl" 242

fileout ::
  OutputType
  -> String
  -> String
fileout PNG =
  (++ ".png")
fileout PDF =
  (++ ".pdf")
fileout PS =
  (++ ".ps")
fileout SVG =
  (++ ".svg")
fileout RenderOnly =
  id

renderout ::
  OutputType 
  -> SizeSpec V2 Double
  -> IO ()
renderout t s =
  let options = CairoOptions (fileout t "out") s t False
  in fst (renderDia Cairo options box)

renderouts ::
  SizeSpec V2 Double
  -> IO ()
renderouts s =
  mapM_ (`renderout` s) [PNG ..]

main ::
  IO ()
main =
  renderouts (mkSizeSpec (V2 (Just 800) Nothing))
