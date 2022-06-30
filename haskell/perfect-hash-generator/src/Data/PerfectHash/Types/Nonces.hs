{-# OPTIONS_HADDOCK hide #-}
module Data.PerfectHash.Types.Nonces where


-- * Types

newtype Nonce = Nonce Int
  deriving Show


-- * Helper functions

mapNonce :: (Int -> Int) -> Nonce -> Nonce
mapNonce f (Nonce x) = Nonce $ f x


isDirectSlot :: Int -> Bool
isDirectSlot val = val < 0