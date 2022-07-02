module Main where

import Control.Monad
import Crypto.Cipher.AES (decryptECB, encryptECB, initAES)
import Data.Bit (castFromWords8, zipBits)
import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr)
import Data.Function
import qualified Data.List as L
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Lib.Util
import System.Random (Random (randomR), mkStdGen, randomRIO)
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
  let winner = _decoded $ L.maximumBy (compare `on` _score) (singleCharXorSolver <$> hexes)
  assertEqual "Challenge 4" "Now that the party is jumping\n" winner

challenge5 :: Assertion
challenge5 = do
  let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  let key = "ICE"
  let expected =
        "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527\
        \2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  assertEqual "Challenge 5" expected (hexEncode $ repeatingKeyXor key input)

challenge6 :: Assertion
challenge6 = do
  vec <- base64Decode . P.concat . P.lines <$> readFile "static/6.txt"
  let keySizes = P.take 3 $ L.sortBy (compare `on` flip sampleBytesDistance vec) [2 .. 40]
  let candidates = do
        keySize <- keySizes
        let t = transposeBytes keySize vec
        let solved = V.map singleCharXorSolver t
        let score = V.sum $ V.map _score solved
        let chars = V.toList $ V.map _char solved
        return (score, chars)
  let (_, key) = L.maximumBy (compare `on` fst) candidates

  assertEqual "Challenge 6" "Terminator X: Bring the noise" key

challenge7 :: Assertion
challenge7 = do
  ciphertext <- C8.pack . byteEncode . base64Decode . P.concat . P.lines <$> readFile "static/7.txt"
  let key = initAES . C8.pack $ "YELLOW SUBMARINE"
  let decrypted = decryptECB key ciphertext
  assertEqual "Challenge 7" "I'm back and I'm ringin' the bell " (C8.unpack . P.head . C8.lines $ decrypted)

challenge8 :: Assertion
challenge8 = do
  inputs <- P.lines <$> readFile "static/8.txt"
  let chunkedCipher = C8.pack . byteEncode . hexDecode <$> inputs
  let repeats = countDupeChunksOf 16 <$> chunkedCipher
  let (winnerRepeats, winner) = L.maximumBy (compare `on` fst) (P.zip repeats inputs)
  assertBool "Challenge 8" (winnerRepeats > 0)
  assertEqual "Challenge 8" "d880" (P.take 4 winner)

challenge9 :: Assertion
challenge9 = do
  let padded = padEOT 20 (C8.pack "YELLOW SUBMARINE")
  assertEqual "Challenge 9" (C8.pack "YELLOW SUBMARINE\EOT\EOT\EOT\EOT") padded

challenge10 :: Assertion
challenge10 = do
  cipher <- C8.pack . byteEncode . base64Decode . P.concat . lines <$> readFile "static/10.txt"
  let iv = B.replicate 16 0
  let key = C8.pack "YELLOW SUBMARINE"
  let decrypted = decryptCBCManual iv key cipher
  assertEqual "Challenge 10" "I'm back and I'm ringin' the bell " (C8.unpack . P.head . C8.lines $ decrypted)

encryptChallenge11 :: ByteString -> IO (Bool, ByteString)
encryptChallenge11 plaintext = do
  iv <- randomByteString 16
  key <- randomByteString 16
  paddingLength <- randomRIO (5, 10)
  padding <- randomByteString paddingLength
  let plaintext' = padEOTToMultipleOf 16 $ B.concat [padding, plaintext, padding]
  useCBC <- coinFlip
  let cipher =
        if useCBC
          then encryptCBCManual iv key plaintext'
          else encryptECB (initAES key) plaintext'

  return (useCBC, cipher)

guessECB :: ByteString -> Bool
guessECB b = countDupeChunksOf 16 b > 0

guessCBC :: ByteString -> Bool
guessCBC = not . guessECB

challenge11 :: Assertion
challenge11 = do
  (usedCBC, cipher) <- encryptChallenge11 (C8.replicate 48 'A')
  assertEqual "Challenge 11" usedCBC (guessCBC cipher)

challenge12Encrypt :: ByteString -> ByteString
challenge12Encrypt plaintext = encryptECB key plaintext'
  where
    key = initAES . B.pack . P.take 16 . L.unfoldr (Just . randomR (0, 255)) $ mkStdGen 300
    unknown =
      C8.pack . byteEncode . base64Decode $
        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGll\
        \cyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    plaintext' = padEOTToMultipleOf 16 $ B.append plaintext unknown

challenge12Decrypt :: Int -> Int -> ByteString
challenge12Decrypt blockSize len = challenge12Decrypt' blockSize len B.empty

challenge12Decrypt' :: Int -> Int -> ByteString -> ByteString
challenge12Decrypt' blockSize cipherLen matched
  | cipherLen == B.length matched = matched
  | otherwise = challenge12Decrypt' blockSize cipherLen (B.concat [matched, match])
  where
    blockNumber = (cipherLen - B.length matched - 1) `div` blockSize
    getBlock = B.take blockSize . B.drop (blockNumber * blockSize)
    input = C8.replicate (cipherLen - B.length matched - 1) 'A'
    res = getBlock . challenge12Encrypt $ input
    match = P.head $ do
      c <- B.pack . L.singleton <$> [0 .. 255]
      let candidate = getBlock . challenge12Encrypt $ B.concat [input, matched, c]
      if candidate == res
        then return c
        else mempty

challenge12 :: Assertion
challenge12 = do
  let ciphers = [challenge12Encrypt $ C8.replicate k 'A' | k <- [0 ..]]
  let lengths = B.length <$> ciphers
  let blockSize = P.head . P.dropWhile (== 0) $ P.zipWith (-) (P.tail lengths) lengths
  assertEqual "Challenge 12 - blockSize discovery" 16 blockSize
  assertEqual "Challenge 12 - detect ECB" True (guessECB $ ciphers !! (2 * blockSize))
  let cipherLen = P.head lengths
  let decrypted = C8.unpack . B.filter (/= 0) $ challenge12Decrypt blockSize cipherLen
  assertEqual "Challenge 12" "Rollin' in my 5." decrypted

main :: IO ()
main = do
  let tests =
        TestList
          [ TestCase challenge1,
            TestCase challenge2,
            TestCase challenge3,
            TestCase challenge4,
            TestCase challenge5,
            TestCase challenge6,
            TestCase challenge7,
            TestCase challenge8,
            TestCase challenge9,
            TestCase challenge10,
            TestCase challenge11,
            TestCase challenge12
          ]
  runTestTT tests
  return ()
