{-# OPTIONS_GHC -Wall #-}

module Lib.Encode.Base64
  ( decode,
    encode,
  )
where

import Data.Bits
import Data.Char
import qualified Data.Map as M hiding (mapMaybe)
import Data.List as L
import Data.List.Split (chunksOf)
import Data.Maybe as M
import Data.Word
import Data.ByteString as B
import Prelude as P
import Lib.Util (reverseMap)

base64Map :: M.Map Word8 Word8
base64Map = M.fromList [(f k, k) | k <- [0 .. 63]]
  where
    f :: Word8 -> Word8
    f w
      | w < 26 = fromIntegral $ ord 'A' + fromIntegral w
      | w < 52 = fromIntegral $ ord 'a' + fromIntegral w - 26
      | w < 62 = fromIntegral $ ord '0' + fromIntegral w - 52
      | w == 62 = fromIntegral $ ord '+'
      | otherwise = fromIntegral $ ord '/'

-- TODO still a nastyass implementation
decode :: ByteString -> ByteString
decode s
  | len `rem` 4 /= 0 = error $ "Unexpected length: " <> show len
  | otherwise =
    stripNULs
      . B.pack
      . P.concatMap f
      . chunksOf 4
      . M.mapMaybe (`M.lookup` base64Map)
      . B.unpack
      $ s
  where
    len = B.length s

    f [a] = f [a, 0, 0, 0]
    f [a, b] = f [a, b, 0, 0]
    f [a, b, c] = f [a, b, c, 0]
    f [a, b, c, d] = [x, y, z]
      where
        x = shiftL a 2 .|. shiftR b 4
        y = shiftL (b .&. 15) 4 .|. shiftR c 2
        z = shiftL (c .&. 3) 6 .|. d
    f x = error $ "unexpected chunk different than 4: " <> show x

    stripNULs = B.reverse . B.dropWhile (== 0) . B.reverse

encode :: ByteString -> ByteString
encode = B.pack . L.foldl' go [] . chunksOf 3 . B.unpack
  where
    reversedMap = reverseMap base64Map

    go acc [a] = go acc [a, 61, 61]
    go acc [a, b] = go acc [a, b, 61]
    go acc [a, b, c] = acc <> M.mapMaybe (`M.lookup` reversedMap) [w, x, y, z]
      where
        w = shiftR a 2
        x = shiftL (a .&. 3) 4 .|. shiftR b 4
        y = shiftL (b .&. 15) 2 .|. shiftR c 6
        z = c .&. 63
    go _ _ = error "unexpected chunk different than 3"
