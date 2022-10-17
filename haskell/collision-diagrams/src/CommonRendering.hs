{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module CommonRendering where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.BrewerSet
import Data.List.Split (chunksOf)


import qualified HashBitsUsageHistogram
import HashBitsUsageHistogram (BiasIndicator (..))


paddingSize :: Double
paddingSize = 5


colors :: [Kolor]
colors = brewerSet Pastel1 9


textEnvelope val = text val # fontSizeL 15 # withEnvelope (rect 120 30 :: Diagram B)


singleSquare :: HashBitsUsageHistogram.MatrixVal -> Diagram B
singleSquare (HashBitsUsageHistogram.MatrixVal val bias) =
  singleSquareColored colr line_weight $
    show val
  where
    colr = getCellColor bias
    line_weight = case bias of
      Correlation _ -> none
      _ -> 4


singleSquareColored
  :: Colour Double
  -> Measure Double
  -> String
  -> Diagram B
singleSquareColored colr line_weight val = offsetWriting <> enclosure
  where
    enclosure = roundedRect 40 30 5 # fc colr # lw line_weight

    offsetWriting = translateY (negate 10) writing
    writing = text val # fontSizeL 20 # fc black


getCellColor :: BiasIndicator -> Kolor
getCellColor AlwaysSame = sRGB 1 0 1 -- Magenta
getCellColor AlwaysDifferent = sRGB 1 1 0 -- Yellow
getCellColor (Correlation val) = if val > 0
  then sRGB 1 (1 - val) (1 - val)
  else sRGB (1 + val) (1 + val) 1