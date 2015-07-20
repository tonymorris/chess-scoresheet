module Data.Test where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Prelude


logo ::
  IO (Diagram B)
logo =
  do i <- loadImageExt "etc/test.png"
     let l = either text (\k -> image k # sized (dims2D 420 232) # alignL) i
     -- return (alignL l === alignL (rect 1000 20))
     return (vcat' (with & sep .~ 10) [l, rect 1000 20 # alignL])
     -- return (l === rect 1000 20)

renderIt :: 
  OutputType
  -> SizeSpec V2 Double
  -> IO ()
renderIt t s =
  let options = CairoOptions ("/tmp/test.pdf") s t False
  in do l <- logo
        fst (renderDia Cairo options l)

run ::
  IO ()
run =
  renderIt PDF (mkSizeSpec (V2 (Just 800) Nothing))

