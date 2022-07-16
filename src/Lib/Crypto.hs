{-# OPTIONS_GHC -Wall #-}

module Lib.Crypto
  ( decryptCBCManual,
    encryptCBCManual,
    englishScore,
    hammingDistance,
    repeatingKeyXor,
    sampleBytesDistance,
    SingleCharXorSolution (..),
    singleCharXorSolver,
    transposeBytes,
  )
where

import Crypto.Cipher.AES (AES, decryptECB, encryptECB)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr, ord, toUpper)
import Data.Function
import Data.List as L
import Data.List.Split (chunksOf)
import Lib.Util
import Prelude hiding ((++))
import qualified Prelude as P

englishScore :: ByteString -> Int
englishScore = P.sum . P.map (score . toUpper) . C8.unpack
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

allSingleCharXors :: ByteString -> [ByteString]
allSingleCharXors b = byteStringXor b <$> [B.replicate (B.length b) k | k <- [0 .. 255]]

data SingleCharXorSolution = SingleCharXorSolution
  { _decoded :: ByteString,
    _score :: Int,
    _char :: Char
  }

singleCharXorSolver :: ByteString -> SingleCharXorSolution
singleCharXorSolver b = L.maximumBy (compare `on` _score) scored
  where
    candidates = allSingleCharXors b
    toSolution (d, s, c) = SingleCharXorSolution {_decoded = d, _score = s, _char = c}
    scored = P.map toSolution $ P.zip3 candidates (englishScore <$> candidates) [chr i | i <- [0 .. 255]]

repeatingKeyXor :: ByteString -> ByteString -> ByteString
repeatingKeyXor key s = B.pack $ P.zipWith xor s' (cycle key')
  where
    key' = B.unpack key
    s' = B.unpack s

hammingDistance :: ByteString -> ByteString -> Int
hammingDistance = (P.sum .) . B.zipWith f
  where
    f x y = popCount $ x `xor` y

sampleBytesDistance :: ByteString -> Int -> Float
sampleBytesDistance b keySize = fromIntegral s / fromIntegral keySize
  where
    bs = P.take 4 . chunksOfBS keySize $ b
    s = P.sum [hammingDistance x y | x <- bs, y <- bs, x /= y]

transposeBytes :: Int -> ByteString -> [ByteString]
transposeBytes keySize = B.transpose . chunksOfBS keySize

encryptCBCManual :: ByteString -> AES -> ByteString -> ByteString
encryptCBCManual iv key p = B.concat . P.tail $ P.scanl go iv p'
  where
    p' = fmap C8.pack . chunksOf 16 . C8.unpack $ p

    go :: ByteString -> ByteString -> ByteString
    go prev next = encryptECB key (byteStringXor prev next)

decryptCBCManual :: ByteString -> AES -> ByteString -> ByteString
decryptCBCManual iv key c = byteStringXor cipherShifted plain
  where
    plain = decryptECB key c
    cipherShifted = B.append iv (B.take (B.length c - B.length iv) c)
