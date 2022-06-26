{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

singleSquare :: String -> Diagram B
singleSquare val = writing <> enclosure
  where
    enclosure = square 40 # fc blue
    writing = text val # fontSizeL 20 # fc white


diagramLeft = vcat [
      singleSquare "foo"
    , singleSquare "bar"
    , singleSquare ""
    , singleSquare "blah"
    , singleSquare ""
    ]


diagramRight = vcat (replicate 5 $ singleSquare $ show 3)

myDiagram = hcat [diagramLeft, diagramRight]


main = mainWith myDiagram
