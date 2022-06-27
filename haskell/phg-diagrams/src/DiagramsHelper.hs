{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module DiagramsHelper where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.BrewerSet


colors :: [Kolor]
colors = brewerSet Pastel1 9


textEnvelope val = text val # fontSizeL 15 # withEnvelope (rect 120 30 :: Diagram B)


singleSquare :: Colour Double -> String -> Diagram B
singleSquare colr val = writing <> enclosure
  where
    enclosure = roundedRect 40 30 5 # fc colr # lw none
    writing = text val # fontSizeL 20 # fc black


diagramLeft = vcat [vsep 10 [
      sqr "foo"
    , sqr "bar"
    , sqr ""
    , sqr "blah"
    , sqr ""
    ]
  , textEnvelope "intermediate table"
  ]
  where
    sqr = singleSquare (colors !! 4)


diagramRight = vcat [
    vsep 10 items
  , textEnvelope "value table"
  ]
  where
    items = replicate 5 $ singleSquare (colors !! 3) $ show 3

myDiagram = hsep 20 [diagramLeft, diagramRight]
