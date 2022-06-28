module Lib.Util
  ( BitVec (..),
    byteStringXor,
    chunkBy,
    intToBitVec,
    pad,
    padEOT,
    reverseMap,
  )
where

import Data.Bit (Bit (..), castFromWords8, cloneToWords8)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Data.ByteString (ByteString)
import Data.ByteString as B

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

padEOT :: Int -> ByteString -> ByteString
padEOT k b | B.length b < k = B.append b (B.replicate diff 4)
           | otherwise = undefined
  where
    diff = k - B.length b

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

byteStringXor :: ByteString -> ByteString -> ByteString
byteStringXor a b = B.pack $ B.zipWith xor a b
