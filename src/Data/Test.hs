module Data.Test where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Prelude

logo ::
  IO (Diagram B R2)
logo =
  do i <- loadImageExt "etc/test.png"
     let l = either text (\k -> image k # sized (Dims 420 232)) i
     return (l === rect 1000 20)

renderChessScoresheet ::
  OutputType
  -> SizeSpec2D
  -> IO ()
renderChessScoresheet t s =
  let options = CairoOptions ("/tmp/test.pdf") s t False
  in do l <- logo
        fst (renderDia Cairo options l)

run ::
  IO ()
run =
  renderChessScoresheet PDF (mkSizeSpec (Just 800) Nothing)
