module Lib.Crypto
  ( base64Encode,
    hexDecode,
    hexEncode,
  )
where

import Data.Bit (Bit, castFromWords8, cloneToWords8)
import Data.Char (chr, digitToInt, ord)
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Data.Word (Word8)
import Lib.Util (chunkBy)
import Prelude hiding ((++))
import qualified Prelude as P

type BitVec = UV.Vector Bit

hexDecode :: String -> BitVec
hexDecode = P.foldr (go . charToWord8) UV.empty
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

base64Encode :: BitVec -> String
base64Encode v = V.toList $ V.map wordToChar words
  where
    words = V.concatMap (convert . cloneToWords8) (chunkBy 6 v)

    wordToChar :: Word8 -> Char
    wordToChar w
      | w < 26 = chr (ord 'A' + fromIntegral w)
      | w < 52 = chr (ord 'a' + fromIntegral w - 26)
      | w < 62 = chr (ord '0' + fromIntegral w - 52)
      | w == 62 = '+'
      | w == 63 = '/'
      | otherwise = error "Unexpectedly large word8!"

hexEncode :: BitVec -> String
hexEncode v = V.toList $ V.map wordToChar words
  where
    words = V.concatMap (convert . cloneToWords8) (chunkBy 4 v)

    wordToChar :: Word8 -> Char
    wordToChar w
      | w < 10 = chr (ord '0' + fromIntegral w)
      | otherwise = chr (ord 'a' + fromIntegral w - 10)
