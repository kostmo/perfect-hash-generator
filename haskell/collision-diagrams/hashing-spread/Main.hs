module Main where

import Diagrams.Backend.SVG.CmdLine

import qualified RenderMatrix
import qualified HashBitsUsageHistogram


main = mainWith the_diagram
  where
    the_diagram = RenderMatrix.hashBitsDiagram $
      HashBitsUsageHistogram.doThing bit_sizes

    bit_sizes = HashBitsUsageHistogram.BitSizes data_bitwidth 32

    data_bitwidth = 8
