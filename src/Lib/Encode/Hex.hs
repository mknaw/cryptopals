{-# OPTIONS_GHC -Wall #-}

module Lib.Encode.Hex
  ( decode,
    encode,
  )
where

import Data.Bits
import Data.ByteString as B
import Data.Char
import Data.List.Split (chunksOf)
import Data.Word
import Prelude as P

decode :: ByteString -> ByteString
decode s
  | odd len = error $ "Unexpected length: " <> show len
  | otherwise = B.pack . fmap f . chunksOf 2 . fmap hexToDigit . B.unpack $ s
  where
    hexToDigit :: Word8 -> Word8
    hexToDigit h
      | h `P.elem` [48 .. 57] = h - 48 -- 0123456789
      | h `P.elem` [97 .. 102] = h - 87 -- abcdef
      | h `P.elem` [65 .. 70] = h - 55 -- ABCDEF
      | otherwise = error $ "Unexpected char: " <> show (chr . fromIntegral $ h)

    len = B.length s

    f [a, b] = shiftL a 4 .|. b
    f _ = error "unexpectedly got an odd hex string"

encode :: ByteString -> ByteString
encode = B.foldl' go B.empty
  where
    hexChr w
      | w < 10 = w + 48
      | w < 17 = w + 87
      | otherwise = error "unexpected byte > 16"
    go acc w = acc <> B.pack (hexChr <$> [shiftR w 4, w .&. 15])
