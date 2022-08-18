{-# OPTIONS_GHC -Wall #-}

module Lib.Util.Random
  ( randomByteString,
    coinFlip,
    randomSample,
    seededRandomByteString,
  )
where

import Control.Monad as M
import Data.Bits (Bits (shiftR, xor, shiftL, (.&.)))
import Data.ByteString as B
import Data.List as L
import Data.Word
import System.Random
import System.Random.Stateful
import Prelude as P

randomByteString :: Int -> IO ByteString
randomByteString k = do
  rands <- M.replicateM k (randomIO :: IO Word8)
  return $ B.pack rands

coinFlip :: IO Bool
coinFlip = do
  randomIO :: IO Bool

randomSample :: [a] -> IO a
randomSample xs = do
  i <- uniformRM (0, P.length xs - 1) globalStdGen
  return $ xs !! i

seededRandomByteString :: Int -> Int -> ByteString
seededRandomByteString len = B.pack . P.take len . L.unfoldr (Just . randomR (0, 255)) . mkStdGen
