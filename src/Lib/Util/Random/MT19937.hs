{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall #-}

module Lib.Util.Random.MT19937
  ( initMT19937,
    MT19937,
  )
where

import Data.Bits
import Data.Word
import System.Random

data MT19937 = MT19937
  { _xs :: [Word32],
    _i :: Int
  }

initMT19937 :: Word32 -> MT19937
initMT19937 seed = twist $ MT19937 xs 0
  where
    f x i = 0x6C078965 * (x `xor` (x `shiftR` 30)) + i
    xs = scanl f seed [1 .. 623]

twist :: MT19937 -> MT19937
twist (MT19937 xs _) = MT19937 xs' 0
  where
    f a b = a .&. 0x80000000 + b .&. 0x7fffffff
    bits = zipWith f (cycle xs) (tail (cycle xs))
    g x = (x `shiftR` 1) `xor` ((x .&. 1) * 0x9908b0df)
    xAs = g <$> bits
    xs' = zipWith xor (take 624 . drop 397 . cycle $ xs) xAs

temper :: Word32 -> Word32
temper y = foldl f y args
  where
    ops = [shiftR, shiftL, shiftL, shiftR]
    shifts = [11, 7, 15, 18]
    masks = [0xffffffff, 0x9d2c5680, 0xefc60000, 0xffffffff]
    f x (op, shft, mask) = x `xor` (op x shft .&. mask)
    args = zip3 ops shifts masks

instance RandomGen MT19937 where
  genWord32 mt@(MT19937 xs i) = (temper $ xs !! i, mt')
    where
      mt'
        | i == length xs - 1 = twist mt
        | otherwise = MT19937 xs (i + 1)

  split = undefined
