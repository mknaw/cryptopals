module Main where

import Control.Monad
import Data.Bit (castFromWords8, zipBits)
import Data.Bits (shiftL, xor)
import Data.Char (chr)
import Data.Function
import Data.List (maximumBy, sortBy)
import Data.Monoid
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Lib.Util (chunkBy)
import Test.HUnit
import Prelude hiding ((++))
import qualified Prelude as P

challenge1 :: Assertion
challenge1 = do
  let input =
        "49276d206b696c6c696e6720796f757220627261696e206c\
        \696b65206120706f69736f6e6f7573206d757368726f6f6d"
  let encoded = base64Encode . hexDecode $ input
  let output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  assertEqual "Challenge 1" output encoded

challenge2 :: Assertion
challenge2 = do
  let a = hexDecode "1c0111001f010100061a024b53535009181c"
  let b = hexDecode "686974207468652062756c6c277320657965"
  let xord = hexEncode $ zipBits xor a b
  let output = "746865206b696420646f6e277420706c6179"
  assertEqual "Challenge 2" output xord

challenge3 :: Assertion
challenge3 = do
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let expected = "Cooking MC's like a pound of bacon"
  assertEqual "Challenge 3" expected (_decoded . singleCharXorSolver . hexDecode $ input)

challenge4 :: Assertion
challenge4 = do
  -- TODO could probably make it faster
  hexes <- P.map hexDecode . lines <$> readFile "static/4.txt"
  let winner = _decoded $ Data.List.maximumBy (compare `on` _score) (singleCharXorSolver <$> hexes)
  assertEqual "Challenge 4" "Now that the party is jumping\n" winner

challenge5 :: Assertion
challenge5 = do
  let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  let key = "ICE"
  let expected =
        "0b3637272a2b2e63622c2e69692a23693a2a3\
        \c6324202d623d63343c2a2622632427276527\
        \2a282b2f20430a652e2c652a3124333a653e2\
        \b2027630c692b20283165286326302e27282f"
  assertEqual "Challenge 5" expected (hexEncode $ repeatingKeyXor key input)

challenge6 :: Assertion
challenge6 = do
  vec <- base64Decode . P.concat . P.lines <$> readFile "static/6.txt"
  let keySizes = P.take 3 $ sortBy (compare `on` flip sampleBytesDistance vec) [2 .. 40]
  let candidates = do
        keySize <- keySizes
        let t = transposeBytes keySize vec
        let solved = V.map singleCharXorSolver t
        let score = V.sum $ V.map _score solved
        let chars = V.toList $ V.map _char solved
        return (score, chars)
  let (_, key) = Data.List.maximumBy (compare `on` fst) candidates

  assertEqual "Challenge 6" "Terminator X: Bring the noise" key

main :: IO ()
main = do
  let tests =
        TestList
          [ TestCase challenge1,
            TestCase challenge2,
            TestCase challenge3,
            TestCase challenge4,
            TestCase challenge5,
            TestCase challenge6
          ]
  runTestTT tests
  return ()
