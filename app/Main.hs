module Main where

import Control.Monad
import Data.Bit (castFromWords8, zipBits)
import Data.Bits (shiftL, xor)
import Data.Char (chr)
import Data.Function
import Data.List (maximumBy, sortBy)
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


allSingleCharXors :: String -> [BitVec]
allSingleCharXors s = [bits `xor` x | x <- singleChars]
  where
    bits = hexDecode s
    lenBytes = UV.length bits `div` 8
    singleChars = [UV.reverse $ castFromWords8 (UV.fromList (P.replicate lenBytes i)) | i <- [0..255]]

challenge3 =
  TestCase
    ( do
        let xors = allSingleCharXors "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        let candidates = byteEncode <$> xors
        let winner = Data.List.maximumBy (compare `on` englishScore) candidates
        assertEqual "Challenge 3" "Cooking MC's like a pound of bacon" winner
    )

challenge4 =
  TestCase
    ( do
        -- TODO could probably make it faster
        input <- lines <$> readFile "static/4.txt"
        let candidates = byteEncode <$> P.concatMap allSingleCharXors input
        let winner = Data.List.maximumBy (compare `on` englishScore) candidates
        assertEqual "Challenge 4" "Now that the party is jumping\n" winner
    )

challenge5 =
  TestCase
    ( do
        let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        let key = "ICE"
        let expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
        assertEqual "Challenge 4" expected (hexEncode $ repeatingKeyXor key input)
    )

main :: IO ()
main = do
  let tests =
        TestList
          [ challenge1,
            challenge2,
            challenge3,
            challenge4,
            challenge5
          ]
  runTestTT tests
  return ()
