module Main where

import Diagrams.Backend.SVG.CmdLine

import qualified RenderMatrix
import qualified ModulusDiagram
import qualified HashBitsUsageHistogram



main = multiMain [
      ("hash", hash_bits_diagram)
    , ("modulus", modulus_diagram)
    ]
  where
    hash_bits_diagram = RenderMatrix.hashBitsDiagram $
      HashBitsUsageHistogram.generateMatrixForHash hash_diagram_bit_sizes

    hash_diagram_bit_sizes = HashBitsUsageHistogram.BitSizes 8 32

    modulus_diagram = ModulusDiagram.modulusDiagram $
      map (\x -> HashBitsUsageHistogram.generateMatrixForModulus x modulus_diagram_bit_sizes) [1..100]
      

    modulus_diagram_bit_sizes = HashBitsUsageHistogram.BitSizes 8 8
