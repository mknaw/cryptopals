module Lib.Util
  ( BitVec (..),
    byteStringXor,
    chunkBy,
    coinFlip,
    countDupeChunksOf,
    intToBitVec,
    pad,
    padEOT,
    padEOTToMultipleOf,
    parseCookie,
    profileFor,
    randomByteString,
    reverseMap,
  )
where

import Control.Monad as M
import Data.Bit (Bit (..), castFromWords8, cloneToWords8)
import Data.Bits (shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Data.Word (Word8)
import System.Random (randomIO)
import Text.ParserCombinators.Parsec
import Prelude as P

type BitVec = UV.Vector Bit

-- TODO would be good to extract our `BitVec` somewhere.
intToBitVec :: Int -> BitVec
intToBitVec k = go UV.empty k
  where
    go v k
      | k <= 0 = v
      | otherwise = go (UV.cons (Bit (k `mod` 2 == 1)) v) (k `shiftR` 1)

pad :: Int -> BitVec -> BitVec
pad k v
  | diff <= 0 = v
  | otherwise = UV.replicate diff (Bit False) UV.++ v
  where
    diff = k - UV.length v

padEOT :: Int -> ByteString -> ByteString
padEOT k b
  | B.length b < k = B.append b (B.replicate diff 4)
  | otherwise = undefined
  where
    diff = k - B.length b

padEOTToMultipleOf :: Int -> ByteString -> ByteString
padEOTToMultipleOf k b
  | B.length b `rem` k == 0 = b
  | otherwise = padEOT newLength b
  where
    newLength = ((B.length b `div` k) + 1) * k

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

countDupeChunksOf :: Int -> ByteString -> Int
countDupeChunksOf k b = P.length chunks - P.length (L.nub chunks)
  where
    chunks = chunksOf 16 . C8.unpack $ b

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap m = M.fromList [(v, k) | (k, v) <- M.toList m]

byteStringXor :: ByteString -> ByteString -> ByteString
byteStringXor a b = B.pack $ B.zipWith xor a b

randomByteString :: Int -> IO ByteString
randomByteString k = do
  rands <- M.replicateM k (randomIO :: IO Word8)
  return $ B.pack rands

coinFlip :: IO Bool
coinFlip = do
  randomIO :: IO Bool

parseCookie :: String -> Either ParseError (M.Map String String)
parseCookie = parse cookieMap "(unknown)"
  where
    cookieMap :: GenParser Char st (M.Map String String)
    cookieMap = do
      let cookiePair = do
            let token = many1 (noneOf "&=")
            k <- token
            v <- char '=' >> token
            return (k, v)
      M.fromList <$> sepBy cookiePair (char '&')

cleanForCookieEncode :: String -> String
cleanForCookieEncode = P.filter (`P.notElem` ['=', '&'])

profileFor :: ByteString -> ByteString
profileFor email = B.concat [C8.pack "email=", email, C8.pack "&uid=10&role=user"]
