module Main where

import Control.Monad
import Data.Bit (castFromWords8, zipBits)
import Data.Bits (shiftL, xor)
import Data.Char (chr)
import Data.Function
import Data.List (maximumBy)
import Data.Monoid
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Prelude hiding ((++))
import qualified Prelude as P
import Test.HUnit

challenge1 =
  TestCase
    ( do
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let encoded = base64Encode . hexDecode $ input
        let output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        assertEqual "Challenge 1" output encoded
    )

challenge2 =
  TestCase
    ( do
        let a = hexDecode "1c0111001f010100061a024b53535009181c"
        let b = hexDecode "686974207468652062756c6c277320657965"
        let xord = hexEncode $ zipBits xor a b
        let output = "746865206b696420646f6e277420706c6179"
        assertEqual "Challenge 2" output xord
    )

challenge3 =
  TestCase
    ( do
        let bits = hexDecode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let k = UV.length bits `div` 8
        let singleChars = [UV.reverse $ castFromWords8 (UV.fromList (P.replicate k i)) | i <- [0..255]]
        let xors = [bits `xor` x | x <- singleChars]
        let candidates = P.map byteEncode xors
        let winner = Data.List.maximumBy (compare `on` englishScore) candidates
        assertEqual "Challenge 3" "Cooking MC's like a pound of bacon" winner
    )

main :: IO ()
main = do
  let tests =
        TestList
          [ challenge1,
            challenge2,
            challenge3
          ]
  runTestTT tests
  return ()
