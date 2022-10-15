module Main where

import Diagrams.Backend.SVG.CmdLine

import qualified RenderMatrix
import qualified BitsHistogram


main = mainWith the_diagram
  where
    the_diagram = RenderMatrix.myDiagram $
      BitsHistogram.doThing bit_sizes

    bit_sizes = BitsHistogram.BitSizes data_bitwidth 32

    data_bitwidth = 8
