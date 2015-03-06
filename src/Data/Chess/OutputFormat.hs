-- this module exists because `OutputType` includes a `RenderOnly` constructor.
-- The `OutputFormat` data type does not.
--
-- Also, the (#) identifier is used in diagrams, so `formatType` uses it, so
-- that diagrams users don't have to.
module Data.Chess.OutputFormat(
  OutputFormat(PNG', PDF', PS', SVG')
, outputFormat
, (<//>)
, formatType
) where

import Control.Lens(Prism', prism', (#))
import Data.Eq(Eq)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Diagrams.Backend.Cairo(OutputType(PNG, PDF, PS, SVG, RenderOnly))
import Prelude(Bounded, Enum, Show)
import System.FilePath(addExtension, FilePath)

data OutputFormat =
  PDF'
  | PNG'
  | PS'
  | SVG'
  deriving (Eq, Ord, Bounded, Enum, Show)

outputFormat ::
  Prism'
    OutputType
    OutputFormat
outputFormat =
  prism'
    (\t -> case t of
             PNG' -> PNG
             PDF' -> PDF
             PS'  -> PS
             SVG' -> SVG)
    (\t -> case t of
             PNG -> Just PNG'
             PDF -> Just PDF'
             PS  -> Just PS'
             SVG -> Just SVG'
             RenderOnly -> Nothing)

(<//>) :: 
  FilePath
  -> OutputFormat
  -> FilePath
p <//> t =
  addExtension p (case t of
                    PNG' -> "png"
                    PDF' -> "pdf"
                    PS'  -> "ps"
                    SVG' -> "svg")

infixr 5 <//>

formatType ::
  OutputFormat
  -> OutputType
formatType = 
  (outputFormat #)
