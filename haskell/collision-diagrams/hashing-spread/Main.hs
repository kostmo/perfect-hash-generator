module Main where

import Diagrams.Backend.SVG.CmdLine

import qualified RenderMatrix
import qualified ModulusDiagram
import qualified HashBitsUsageHistogram

import Data.PerfectHash.Types.Nonces (Nonce (..))


main = multiMain [
      ("hash", hash_bits_diagram)
    , ("modulus", modulus_diagram)
    ]
  where
    hash_bits_diagram = RenderMatrix.hashBitsDiagram $
      map (\x -> HashBitsUsageHistogram.generateMatrixForHash x hash_diagram_bit_sizes) [
          Nothing
        , Just $ Nonce 1
        , Just $ Nonce 2
        , Just $ Nonce 3
        , Just $ Nonce 4
        , Just $ Nonce 5
        , Just $ Nonce 6
        , Just $ Nonce 7
        ]

    hash_diagram_bit_sizes = HashBitsUsageHistogram.BitSizes 8 32

    modulus_diagram = ModulusDiagram.modulusDiagram $
      map (\x -> HashBitsUsageHistogram.generateMatrixForModulus x modulus_diagram_bit_sizes) [1..100]
      

    modulus_diagram_bit_sizes = HashBitsUsageHistogram.BitSizes 8 10
