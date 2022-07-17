{-# OPTIONS_GHC -Wall #-}

module Lib.Crypto
  ( bsToInteger,
    decryptCBC,
    encryptCBC,
    decryptCTR,
    englishScore,
    incrementBS,
    incrementCTR,
    integerToBS,
    hammingDistance,
    keyStreamCTR,
    padZero,
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr, ord, toUpper)
import Data.Function
import Data.List as L
import Data.List.Split (chunksOf)
import Data.Word
import GHC.ByteOrder
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

encryptCBC :: ByteString -> AES -> ByteString -> ByteString
encryptCBC iv key p = B.concat . P.tail $ P.scanl go iv p'
  where
    p' = fmap C8.pack . chunksOf 16 . C8.unpack $ p

    go :: ByteString -> ByteString -> ByteString
    go prev next = encryptECB key (byteStringXor prev next)

decryptCBC :: ByteString -> AES -> ByteString -> ByteString
decryptCBC iv key c = byteStringXor cipherShifted plain
  where
    plain = decryptECB key c
    cipherShifted = B.append iv (B.take (B.length c - B.length iv) c)

bsToInteger :: ByteString -> Integer
bsToInteger s = P.sum $ P.zipWith (*) [256 ^ i | i <- [0 :: Integer ..]] xs
  where
    xs = fmap fromIntegral . B.unpack . B.reverse $ s

integerToBS :: Integer -> ByteString
integerToBS x
  | x == 0 = B.pack [0]
  | otherwise = B.pack $ go [] x
  where
    go :: [Word8] -> Integer -> [Word8]
    go acc x'
      | x' == 0 = acc
      | otherwise = go (fromIntegral (x' `rem` 256) : acc) (x' `div` 256)

padZero :: Int -> ByteString -> ByteString
padZero k s = B.replicate (k - B.length s) 0 <> s

incrementBS :: ByteString -> ByteString
incrementBS b = integerToBS (bsToInteger b + 1)

incrementCTR :: ByteOrder -> ByteString -> ByteString
incrementCTR bo s = nonce <> inc counter
  where
    nonce = B.take 8 s
    counter = B.drop 8 s
    inc = case bo of
      BigEndian -> padZero 8 . incrementBS
      LittleEndian -> B.reverse . padZero 8 . incrementBS . B.reverse

keyStreamCTR :: ByteOrder -> AES -> BL.ByteString
keyStreamCTR bo key = BL.concat $ BL.fromStrict . encryptECB key <$> iterate (incrementCTR bo) (nonce <> counter)
  where
    nonce = padZero 8 . B.reverse . integerToBS $ 0
    counter = padZero 8 . B.reverse . integerToBS $ 0

encryptCTR :: ByteOrder -> AES -> ByteString -> ByteString
encryptCTR bo key plain = B.pack $ BL.zipWith xor (BL.fromStrict plain) (keyStreamCTR bo key)

decryptCTR :: ByteOrder -> AES -> ByteString -> ByteString
decryptCTR = encryptCTR
