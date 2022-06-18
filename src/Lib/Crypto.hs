module Lib.Crypto
  ( base64Encode,
    byteEncode,
    englishScore,
    hexDecode,
    hexEncode,
  )
where

import Data.Bit (Bit, castFromWords8, cloneToWords8)
import Data.Char (chr, digitToInt, ord)
import qualified Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Data.Word (Word8)
import Lib.Util (chunkBy, reverseMap)
import Prelude hiding ((++))
import qualified Prelude as P

type BitVec = UV.Vector Bit

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
base64Decode = castFromWords8 . UV.mapMaybe (`M.lookup` base64Map) . UV.fromList

base64Encode :: BitVec -> String
base64Encode v = V.toList $ V.mapMaybe (`M.lookup` reversedMap) words
  where
    reversedMap = reverseMap base64Map
    words = V.concatMap (convert . cloneToWords8 . UV.reverse) (chunkBy 6 v)

byteEncode :: BitVec -> String
byteEncode = UV.toList . UV.reverse . UV.map (chr . fromIntegral) . cloneToWords8 . UV.reverse

englishScore :: String -> Int
englishScore = P.sum . P.map (score . ord)
  where
    score i
      | i < 32 = 0
      | i < 33 = 2 -- space
      | i < 48 = 1 -- !"#$%&\'()*+,-./
      | i < 58 = 2 -- 0123456789
      | i < 65 = 1 -- :;<=>?@
      | i < 91 = 4 -- A - Z
      | i < 97 = 1 -- [\]^_`
      | i < 123 = 4 -- a - z
      | otherwise = 0
