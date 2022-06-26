module Data.PerfectHash.Types.Nonces where

import           Data.Default             (Default, def)


-- | This choice of prime number @0x01000193@ was taken from the Python implementation
-- on <http://stevehanov.ca/blog/index.php?id=119 Steve Hanov's page>.
primeFNV :: Int
primeFNV = 0x01000193


newtype Nonce = Nonce Int
  deriving Show

instance Default Nonce where
  def = Nonce 0


mapNonce :: (Int -> Int) -> Nonce -> Nonce
mapNonce f (Nonce x) = Nonce $ f x


isDirectSlot :: Nonce -> Bool
isDirectSlot (Nonce val) = val < 0


-- | Used in the 'hash' function
getNonzeroNonceVal :: Nonce -> Int
getNonzeroNonceVal (Nonce nonce) =
  if nonce == 0
    then primeFNV
    else nonce