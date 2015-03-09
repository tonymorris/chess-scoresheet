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

blackshading ::
  Colour Double
blackshading =
  sRGB24 217 217 217

logo ::
  IO (Diagram B R2)
logo =
  fmap (either text (\l -> image l # sized (Dims 150 150))) (loadImageExt "etc/tgcc-logo.png")

logowithurl ::
  IO (Diagram B R2)
logowithurl =
  do l <- logo
     let t = text "http://thegapchessclub.org.au" # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
         r = rect 200 10 # lc white
     return (((t <> r) === l) # alignR)

namebox ::
  Diagram B R2
namebox =
  let nametext c = alignedText (-0.1) 0 c # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
      ratingtext = alignedText (-0.1) 0 "rating" # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc darkblue
      name c s = nametext c <> rect 300 20 # lc darkblue # fc s # alignL
      rating s = ratingtext <> rect 78 20 # lc darkblue # fc s # alignL
      box c s = hcat' (with & sep .~ 20) [name c s, rating s] # centerX
  in box "white" white === box "black" blackshading
  
row ::
  Int
  -> Diagram B R2
row r =
  let numbertext n = alignedText 1.2 0.5 (show n) # font "DejaVu Sans Mono" # fontSizeN 0.01 # fc white
      number n = numbertext n <> rect 20 20 # alignR # fc darkblue # lc darkblue
      whitemove = rect 60 20 # lc darkblue
      blackmove = rect 60 20 # fc (sRGB24 192 192 192) # lc darkblue
      time = text ":" # fontSizeN 0.03 # fc darkblue <> rect 30 20 # fc blackshading # lc darkblue
  in number r ||| whitemove ||| time ||| blackmove ||| time # centerX

row' ::
  [Int]
  -> Diagram B R2
row' =
  foldr ((===) . row) mempty

rows1 ::
  Diagram B R2
rows1 =
  (row' [1..25] ||| row' [26..50]) # centerX

scoresheet ::
  Diagram B R2
  -> Diagram B R2
scoresheet l =
   vcat' (with & sep .~ 10) [l, namebox, rows1]
  
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

run ::
  IO ()
run =
  renderChessScoresheet PDF' (mkSizeSpec (Just 800) Nothing)
