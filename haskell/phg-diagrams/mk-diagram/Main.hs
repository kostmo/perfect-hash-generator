{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Backend.SVG.CmdLine

import DiagramsHelper


main = mainWith myDiagram
