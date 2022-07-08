module Main where

import Control.Monad
import Crypto.Cipher.AES (AES, decryptECB, encryptECB, initAES)
import Data.Bit (castFromWords8, zipBits)
import Data.Bits (shiftL, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr)
import Data.Either
import Data.Function
import qualified Data.List as L
import Data.List.Split (chunksOf)
import Data.Map as M
import Data.Monoid
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Lib.Crypto.ECBOracle
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

challenge12Oracle :: ByteString -> ByteString
challenge12Oracle input = encryptECB key plaintext
  where
    key = initAES (seededRandomByteString 16 300)
    target =
      C8.pack . byteEncode . base64Decode $
        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGll\
        \cyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    plaintext = padEOTToMultipleOf 16 $ B.append input target

challenge12Decrypted :: String
challenge12Decrypted =
  "Rollin' in my 5.0\n\
  \With my rag-top down so my hair can blow\n\
  \The girlies on standby waving just to say hi\n\
  \Did you stop? No, I just drove by\n"

challenge12 :: Assertion
challenge12 = do
  let ciphers = [challenge12Oracle $ C8.replicate k 'A' | k <- [0 ..]]
  let blockSize = detectBlockSize challenge12Oracle
  assertEqual "Challenge 12 - blockSize discovery" 16 blockSize
  assertEqual "Challenge 12 - detect ECB" True (guessECB $ ciphers !! (2 * blockSize))
  let cipherLen = detectCipherLen challenge12Oracle
  let decrypted = C8.unpack . B.takeWhile (`P.notElem` [0, 4]) $ byteAtATimeDecrypt challenge12Oracle
  assertEqual "Challenge 12" challenge12Decrypted decrypted

challenge13Key :: AES
challenge13Key = initAES (seededRandomByteString 16 666)

challenge13Encrypt :: ByteString -> ByteString
challenge13Encrypt plaintext = encryptECB challenge13Key plaintext'
  where
    plaintext' = padEOTToMultipleOf 16 (profileFor plaintext)

challenge13Decrypt :: ByteString -> M.Map String String
challenge13Decrypt cipher = fromRight M.empty (parseCookie $ C8.unpack decrypted')
  where
    decrypted = decryptECB challenge13Key cipher
    decrypted' = B.reverse . B.dropWhile (== 0) . B.reverse $ decrypted

challenge13 :: Assertion
challenge13 = do
  let input = "foo=bar&baz=qux&zap=zazzle"
  let parsed = fromRight M.empty (parseCookie input)
  let expected = M.fromList [("foo", "bar"), ("baz", "qux"), ("zap", "zazzle")]
  assertEqual "Challenge 13 - parse cookies" expected parsed

  let encoded = profileFor . C8.pack $ "foo@bar.com"
  assertEqual "Challenge 13 - encode profile" (C8.pack "email=foo@bar.com&uid=10&role=user") encoded

  -- email=hello@sup. admin            com&uid=10&role= user
  -- ................ ................ ................ ................
  let input = B.concat [C8.pack "hello@sup.admin", B.replicate (16 - 5) 0, C8.pack "com"]
  let cipher = challenge13Encrypt input
  let fakeCipher =
        B.concat
          [ B.take 16 cipher, -- email=hello@sup.
            B.take 16 (B.drop 32 cipher), -- com&uid=10&role=
            B.take 16 (B.drop 16 cipher) -- admin
          ]
  assertEqual "Challenge 13" (Just "admin") (M.lookup "role" (challenge13Decrypt fakeCipher))

challenge14Oracle :: ByteString -> ByteString
challenge14Oracle input = encryptECB key plaintext
  where
    key = initAES $ seededRandomByteString 16 404
    len = fst . randomR (10, 255) $ mkStdGen 300
    prefix = seededRandomByteString len 200
    target =
      C8.pack . byteEncode . base64Decode $
        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGll\
        \cyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    plaintext = padEOTToMultipleOf 16 $ B.concat [prefix, input, target]

challenge14 :: Assertion
challenge14 = do
  let decrypted = C8.unpack . B.takeWhile (`P.notElem` [0, 4]) $ byteAtATimeDecrypt challenge14Oracle
  assertEqual "Challenge 14" challenge12Decrypted decrypted

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
            TestCase challenge12,
            TestCase challenge13,
            TestCase challenge14
          ]
  runTestTT tests
  return ()
