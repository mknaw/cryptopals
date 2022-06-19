module Lib.Crypto
  ( base64Decode,
    base64Encode,
    byteDecode,
    byteEncode,
    englishScore,
    hammingDistance,
    hexDecode,
    hexEncode,
    repeatingKeyXor,
    sampleBytesDistance,
    SingleCharXorSolution (..),
    singleCharXorSolver,
    transposeBytes,
  )
where

import Data.Bit (Bit, castFromWords8, cloneToWords8, reverseBits, zipBits)
import Data.Bits (xor)
import Data.Char (chr, digitToInt, ord, toUpper)
import Data.Function
import Data.List (maximumBy, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M hiding (mapMaybe)
import qualified Data.Maybe as M (mapMaybe)
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Data.Word (Word8)
import Lib.Util
import Text.Printf (vFmt)
import Prelude hiding ((++))
import qualified Prelude as P

hexDecode :: String -> BitVec
hexDecode = UV.reverse . P.foldr (go . charToWord8) UV.empty
  where
    charToWord8 :: Char -> Word8
    charToWord8 'a' = 10
    charToWord8 'b' = 11
    charToWord8 'c' = 12
    charToWord8 'd' = 13
    charToWord8 'e' = 14
    charToWord8 'f' = 15
    charToWord8 c = fromIntegral $ digitToInt c

    go :: Word8 -> BitVec -> BitVec
    go el acc = acc UV.++ next4
      where
        next4 = UV.take 4 $ castFromWords8 (UV.fromList [el])

hexEncode :: BitVec -> String
hexEncode v = V.toList $ V.map wordToChar words
  where
    words = V.concatMap (convert . cloneToWords8 . UV.reverse) (chunkBy 4 v)

    wordToChar :: Word8 -> Char
    wordToChar w
      | w < 10 = chr (ord '0' + fromIntegral w)
      | otherwise = chr (ord 'a' + fromIntegral w - 10)

base64Map :: M.Map Char Word8
base64Map = M.fromList [(f k, k) | k <- [0 .. 63]]
  where
    f :: Word8 -> Char
    f w
      | w < 26 = chr (ord 'A' + fromIntegral w)
      | w < 52 = chr (ord 'a' + fromIntegral w - 26)
      | w < 62 = chr (ord '0' + fromIntegral w - 52)
      | w == 62 = '+'
      | otherwise = '/'

base64Decode :: String -> BitVec
base64Decode s = v'
  where
    v = UV.concat . P.map (pad 6 . intToBitVec . fromIntegral) . M.mapMaybe (`M.lookup` base64Map) $ s
    -- Discard any padding
    len = UV.length v
    v' = UV.take (len - len `mod` 8) v

base64Encode :: BitVec -> String
base64Encode v = V.toList $ V.mapMaybe (`M.lookup` reversedMap) words
  where
    reversedMap = reverseMap base64Map
    words = V.concatMap (convert . cloneToWords8 . UV.reverse) (chunkBy 6 v)

byteDecode :: String -> BitVec
byteDecode s = reverseBits $ castFromWords8 (UV.fromList $ fromIntegral . ord <$> P.reverse s)

byteEncode :: BitVec -> String
byteEncode = UV.toList . UV.reverse . UV.map (chr . fromIntegral) . cloneToWords8 . UV.reverse

englishScore :: String -> Int
englishScore = P.sum . P.map (score . toUpper)
  where
    score i
      -- Weighted by approximate frequency in English text.
      | i `P.elem` "E" = 12
      | i `P.elem` "T" = 9
      | i `P.elem` "AINOS" = 8
      | i `P.elem` "HR " = 6
      | i `P.elem` "DL" = 4
      | i `P.elem` "UCMF" = 3
      | i `P.elem` "WYGPB" = 2
      | i `P.elem` "VK" = 2
      | i `P.elem` "QJXZ" = 2
      | ord i < 20 = -10
      | otherwise = 0

allSingleCharXors :: BitVec -> [BitVec]
allSingleCharXors vec = [vec `xor` x | x <- singleChars]
  where
    lenBytes = UV.length vec `div` 8
    singleChars = [UV.reverse $ castFromWords8 (UV.fromList (P.replicate lenBytes i)) | i <- [0 .. 255]]

data SingleCharXorSolution = SingleCharXorSolution
  { _decoded :: String,
    _score :: Int,
    _char :: Char
  }

singleCharXorSolver :: BitVec -> SingleCharXorSolution
singleCharXorSolver v = Data.List.maximumBy (compare `on` _score) scored
  where
    candidates = byteEncode <$> allSingleCharXors v
    toSolution (d, s, c) = SingleCharXorSolution {_decoded = d, _score = s, _char = c}
    scored = P.map toSolution $ P.zip3 candidates (englishScore <$> candidates) [chr i | i <- [0 .. 255]]

repeatingKeyXor :: String -> String -> BitVec
repeatingKeyXor key s = UV.fromList $ P.zipWith xor s' (cycle key')
  where
    key' = UV.toList $ byteDecode key
    s' = UV.toList $ byteDecode s

hammingDistance :: BitVec -> BitVec -> Int
hammingDistance a b = UV.sum (UV.zipWith isDifferent a b)
  where
    isDifferent :: Bit -> Bit -> Int
    isDifferent x y = if x == y then 0 else 1

sampleBytesDistance :: Int -> BitVec -> Float
sampleBytesDistance keySize v = fromIntegral s / fromIntegral keySize
  where
    -- Would blow up for a small `BitVec`, but whatever
    a : b : c : d : _ = V.toList $ chunkBy (keySize * 8) v
    s = P.sum [hammingDistance x y | x <- [a, b, c, d], y <- [a, b, c, d], x /= y]

transposeBytes :: Int -> BitVec -> V.Vector BitVec
transposeBytes keySize v = V.fromList $ UV.concat <$> chunks
  where
    -- TODO there's probably a faster, `Vector`-oriented way to do this, without going to list.
    chunks = transpose $ chunksOf keySize $ V.toList (chunkBy 8 v)
