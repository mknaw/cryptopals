module Lib.Util
  ( chunkBy,
  )
where

import Data.Bit (Bit, castFromWords8, cloneToWords8)
import Data.Vector as V
import Data.Vector.Unboxed as UV

type BitVec = UV.Vector Bit

-- TODO should be able to take generic(-er) args?
chunkBy :: UV.Unbox a => Int -> UV.Vector a -> V.Vector (UV.Vector a)
chunkBy k v = chunkBy' V.empty k v
  where
    chunkBy' :: UV.Unbox a => V.Vector (UV.Vector a) -> Int -> UV.Vector a -> V.Vector (UV.Vector a)
    chunkBy' acc k v =
      if UV.null xs'
        then acc'
        else chunkBy' acc' k xs'
      where
        (xs, xs') = UV.splitAt k v
        acc' = V.cons xs acc
