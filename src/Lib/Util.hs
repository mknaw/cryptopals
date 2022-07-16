{-# OPTIONS_GHC -Wall #-}

module Lib.Util
  ( byteStringXor,
    chunksOfBS,
    commonPrefix,
    countDupeChunksOf,
    padPKCS7,
    padPKCS7',
    parseKeyValue,
    profileFor,
    pseudoUrlEncode,
    reverseMap,
    shiftBy,
    stripPKCS7,
  )
where

import Control.Monad as M
import Data.Bits
import Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Prelude as P

shiftBy :: Int -> [a] -> [a]
shiftBy k xs = bs <> as
  where
    (as, bs) = L.splitAt (P.length xs - k) xs

padPKCS7' :: Int -> ByteString -> ByteString
padPKCS7' blockSize b = B.append b (B.replicate r $ fromIntegral r)
  where
    r = blockSize - (B.length b `mod` blockSize)

padPKCS7 :: ByteString -> ByteString
padPKCS7 = padPKCS7' 16

stripPKCS7 :: ByteString -> Maybe ByteString
stripPKCS7 b
  | B.length padding /= fromIntegral c = Nothing
  | otherwise = Just $ B.reverse b''
  where
    b' = B.reverse b
    c = B.head b'
    (padding, b'') = B.span (== c) b'

chunksOfBS :: Int -> ByteString -> [ByteString]
chunksOfBS k = fmap B.pack . chunksOf k . B.unpack

countDupeChunksOf :: Int -> ByteString -> Int
countDupeChunksOf k b = P.length chunks - P.length (L.nub chunks)
  where
    chunks = chunksOf k . C8.unpack $ b

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap m = M.fromList [(v, k) | (k, v) <- M.toList m]

byteStringXor :: ByteString -> ByteString -> ByteString
byteStringXor a b = B.pack $ B.zipWith xor a b

parseKeyValue :: Char -> String -> Either ParseError (M.Map String String)
parseKeyValue delim = parse keyValue "(unknown)"
  where
    keyValue :: GenParser Char st (M.Map String String)
    keyValue = do
      let pair = do
            let tok = many1 (noneOf (delim : "="))
            k <- tok
            v <- char '=' >> tok
            return (k, v)
      M.fromList <$> sepBy pair (char delim)

pseudoUrlEncode :: ByteString -> ByteString
pseudoUrlEncode = B.foldl' go B.empty
  where
    go b c = B.append b (safe c)
    safe c
      | c == 61 = C8.pack "%3D" -- =
      | c == 59 = C8.pack "%3B" -- ;
      | c == 38 = C8.pack "%26" -- &
      | otherwise = B.pack [c]

profileFor :: ByteString -> ByteString
profileFor email = B.concat [C8.pack "email=", pseudoUrlEncode email, C8.pack "&uid=10&role=user"]

commonPrefix :: ByteString -> ByteString -> ByteString
commonPrefix a b = B.pack . fmap fst . P.takeWhile (uncurry (==)) $ B.zip a b
