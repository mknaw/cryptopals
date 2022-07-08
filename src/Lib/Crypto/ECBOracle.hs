module Lib.Crypto.ECBOracle
  ( byteAtATimeDecrypt,
    detectBlockSize,
    detectCipherLen,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString as B
import Data.ByteString.Char8 as C8
import Data.List as L
import Lib.Util (commonPrefix)

type Oracle = ByteString -> ByteString

detectBlockSize :: Oracle -> Int
detectBlockSize oracle = L.head . L.dropWhile (== 0) $ diffs
  where
    lengths = B.length <$> [oracle $ C8.replicate k 'A' | k <- [0 ..]]
    diffs = L.zipWith (-) (L.tail lengths) lengths

detectCipherLen :: Oracle -> Int
detectCipherLen oracle = B.length (oracle B.empty)

detectOraclePrefixLen :: Oracle -> Int
detectOraclePrefixLen oracle = maybe fullBlockLen (fullBlockLen + blockSize -) detected
  where
    blockSize = detectBlockSize oracle
    ciphers = [oracle $ C8.replicate k 'A' | k <- [0 .. blockSize]]
    zipped = L.zipWith commonPrefix (L.tail ciphers) ciphers
    fullBlockLen = B.length . L.head $ zipped
    detected = L.findIndex (not . B.null) . fmap (B.drop fullBlockLen) $ zipped

byteAtATimeDecrypt :: Oracle -> ByteString
byteAtATimeDecrypt oracle = byteAtATimeDecrypt' oracle blockSize cipherLen prefixLen B.empty
  where
    blockSize = detectBlockSize oracle
    cipherLen = detectCipherLen oracle
    prefixLen = detectOraclePrefixLen oracle

byteAtATimeDecrypt' :: Oracle -> Int -> Int -> Int -> ByteString -> ByteString
byteAtATimeDecrypt' oracle blockSize cipherLen prefixLen matched
  | L.null matches = matched
  | cipherLen - prefixLen == B.length matched = matched
  | otherwise = byteAtATimeDecrypt' oracle blockSize cipherLen prefixLen next
  where
    prefixOffset = (prefixLen + blockSize - 1) `div` blockSize -- quasi round-up `div`
    blockNumber = (cipherLen `div` blockSize) + prefixOffset - 1
    getBlock = B.take blockSize . B.drop (blockNumber * blockSize)
    input = C8.replicate (cipherLen - B.length matched + (prefixOffset * blockSize) - prefixLen - 1) 'A'
    res = getBlock . oracle $ input
    matches = do
      char <- B.pack . L.singleton <$> [0 .. 255]
      let candidate = getBlock . oracle $ B.concat [input, matched, char]
      if candidate == res
        then return char
        else mempty
    next = B.concat [matched, L.head matches]
