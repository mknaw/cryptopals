module Lib.Util
  ( BitVec (..),
    chunkBy,
    intToBitVec,
    pad,
    reverseMap,
  )
where

import Data.Bit (Bit (..), castFromWords8, cloneToWords8)
import Data.Bits (shiftR)
import qualified Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as UV

type BitVec = UV.Vector Bit

-- TODO would be good to extract our `BitVec` somewhere.
intToBitVec :: Int -> BitVec
intToBitVec k = go UV.empty k
  where
    go v k
      | k <= 0 = v
      | otherwise = go (UV.cons (Bit (k `mod` 2 == 1)) v) (k `shiftR` 1)

pad :: Int -> BitVec -> BitVec
pad k v | diff <= 0 = v
        | otherwise = UV.replicate diff (Bit False) UV.++ v
  where
    diff = k - UV.length v

-- TODO would be nice if we didn't have to do the Unbox / Box stuff
-- but dunno how to provide an output type
chunkBy :: UV.Unbox a => Int -> UV.Vector a -> V.Vector (UV.Vector a)
chunkBy k v = V.reverse $ chunkBy' V.empty k v
  where
    chunkBy' :: UV.Unbox a => V.Vector (UV.Vector a) -> Int -> UV.Vector a -> V.Vector (UV.Vector a)
    chunkBy' acc k v =
      if UV.null xs'
        then acc'
        else chunkBy' acc' k xs'
      where
        (xs, xs') = UV.splitAt k v
        acc' = V.cons xs acc

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap m = M.fromList [(v, k) | (k, v) <- M.toList m]
