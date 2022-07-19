{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Crypto.Cipher.AES (AES, decryptECB, encryptECB, initAES)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Either
import Data.Function
import qualified Data.List as L
import Data.Map as M
import Data.Maybe (isJust, isNothing)
import GHC.ByteOrder
import Lib.Crypto
import Lib.Crypto.ECBOracle
import qualified Lib.Encode.Base64 as B64
import qualified Lib.Encode.Hex as HEX
import Lib.Util
import Lib.Util.Random
import System.Random (Random (randomR), mkStdGen, randomRIO)
import Test.HUnit
import Prelude hiding ((++))
import qualified Prelude as P

challenge1 :: Assertion
challenge1 = do
  let input =
        "49276d206b696c6c696e6720796f757220627261696e206c\
        \696b65206120706f69736f6e6f7573206d757368726f6f6d"
  let encoded = B64.encode . HEX.decode $ input
  let output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  assertEqual "Challenge 1" output encoded

challenge2 :: Assertion
challenge2 = do
  let a = HEX.decode "1c0111001f010100061a024b53535009181c"
  let b = HEX.decode "686974207468652062756c6c277320657965"
  let xord = byteStringXor a b
  assertEqual "Challenge 2" "the kid don't play" xord

challenge3 :: Assertion
challenge3 = do
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let expected = "Cooking MC's like a pound of bacon"
  assertEqual "Challenge 3" expected (_decoded . singleCharXorSolver . HEX.decode $ input)

challenge4 :: Assertion
challenge4 = do
  -- TODO could probably make it faster
  hexes <- P.map (HEX.decode . C8.pack) . lines <$> readFile "static/4.txt"
  let winner = _decoded $ L.maximumBy (compare `on` _score) (singleCharXorSolver <$> hexes)
  assertEqual "Challenge 4" "Now that the party is jumping\n" winner

challenge5 :: Assertion
challenge5 = do
  let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  let key = "ICE"
  let expected =
        "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527\
        \2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  assertEqual "Challenge 5" expected (HEX.encode $ repeatingKeyXor key input)

challenge6 :: Assertion
challenge6 = do
  b <- B64.decode . C8.pack . P.concat . P.lines <$> readFile "static/6.txt"
  let keySizes = P.take 3 $ L.sortBy (compare `on` sampleBytesDistance b) [2 .. 40]
  let candidates = do
        keySize <- keySizes
        let t = transposeBytes keySize b
        let solved = singleCharXorSolver <$> t
        let score = P.sum $ _score <$> solved
        let chars = _char <$> solved
        return (score, chars)
  let (_, key) = L.maximumBy (compare `on` fst) candidates

  assertEqual "Challenge 6" "Terminator X: Bring the noise" key

challenge7 :: Assertion
challenge7 = do
  ciphertext <- B64.decode . C8.pack . P.concat . P.lines <$> readFile "static/7.txt"
  let key = initAES . C8.pack $ "YELLOW SUBMARINE"
  let decrypted = decryptECB key ciphertext
  assertEqual "Challenge 7" "I'm back and I'm ringin' the bell " (P.head . C8.lines $ decrypted)

challenge8 :: Assertion
challenge8 = do
  inputs <- P.lines <$> readFile "static/8.txt"
  let chunkedCipher = HEX.decode . C8.pack <$> inputs
  let repeats = countDupeChunksOf 16 <$> chunkedCipher
  let (winnerRepeats, winner) = L.maximumBy (compare `on` fst) (P.zip repeats inputs)
  assertBool "Challenge 8" (winnerRepeats > 0)
  assertEqual "Challenge 8" "d880" (P.take 4 winner)

challenge9 :: Assertion
challenge9 = do
  let padded = padPKCS7' 20 "YELLOW SUBMARINE"
  assertEqual "Challenge 9" "YELLOW SUBMARINE\EOT\EOT\EOT\EOT" padded

challenge10 :: Assertion
challenge10 = do
  cipher <- B64.decode . C8.pack . P.concat . lines <$> readFile "static/10.txt"
  let iv = B.replicate 16 0
  let key = initAES $ C8.pack "YELLOW SUBMARINE"
  let decrypted = decryptCBC iv key cipher
  assertEqual "Challenge 10" "I'm back and I'm ringin' the bell " (C8.unpack . P.head . C8.lines $ decrypted)

encryptChallenge11 :: ByteString -> IO (Bool, ByteString)
encryptChallenge11 plaintext = do
  iv <- randomByteString 16
  key <- initAES <$> randomByteString 16
  paddingLength <- randomRIO (5, 10)
  padding <- randomByteString paddingLength
  let plaintext' = padPKCS7 $ B.concat [padding, plaintext, padding]
  useCBC <- coinFlip
  let cipher =
        if useCBC
          then encryptCBC iv key plaintext'
          else encryptECB key plaintext'

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
      B64.decode
        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGll\
        \cyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    plaintext = padPKCS7 $ B.append input target

challenge12Decrypted :: ByteString
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
  let decrypted = stripPKCS7 $ byteAtATimeDecrypt challenge12Oracle
  assertEqual "Challenge 12" (Just challenge12Decrypted) decrypted

challenge13Key :: AES
challenge13Key = initAES (seededRandomByteString 16 666)

challenge13Encrypt :: ByteString -> ByteString
challenge13Encrypt plaintext = encryptECB challenge13Key plaintext'
  where
    plaintext' = padPKCS7 (profileFor plaintext)

challenge13Decrypt :: ByteString -> M.Map String String
challenge13Decrypt cipher = fromRight M.empty (parseKeyValue '&' $ C8.unpack decrypted')
  where
    decrypted = decryptECB challenge13Key cipher
    decrypted' = B.reverse . B.dropWhile (== 0) . B.reverse $ decrypted

challenge13 :: Assertion
challenge13 = do
  let input = "foo=bar&baz=qux&zap=zazzle"
  let parsed = fromRight M.empty (parseKeyValue '&' input)
  let expected = M.fromList [("foo", "bar"), ("baz", "qux"), ("zap", "zazzle")]
  assertEqual "Challenge 13 - parse cookies" expected parsed

  let encoded = profileFor "foo@bar.com"
  assertEqual "Challenge 13 - encode profile" "email=foo@bar.com&uid=10&role=user" encoded

  -- email=hello@sup. admin            com&uid=10&role= user
  -- ................ ................ ................ ................
  let input' = B.concat ["hello@sup.admin", B.replicate (16 - 5) 0, "com"]
  let cipher = challenge13Encrypt input'
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
      B64.decode
        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGll\
        \cyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    plaintext = padPKCS7 $ B.concat [prefix, input, target]

challenge14 :: Assertion
challenge14 = do
  let decrypted = stripPKCS7 $ byteAtATimeDecrypt challenge14Oracle
  assertEqual "Challenge 14" (Just challenge12Decrypted) decrypted

challenge15 :: Assertion
challenge15 = do
  let input = "ICE ICE BABY\x04\x04\x04\x04"
  assertEqual "Challenge 15" (Just "ICE ICE BABY") (stripPKCS7 input)
  let input' = "ICE ICE BABY\x05\x05\x05\x05"
  assertBool "Challenge 15" (isNothing $ stripPKCS7 input')
  let input'' = "ICE ICE BABY\x01\x02\x03\x04"
  assertBool "Challenge 15" (isNothing $ stripPKCS7 input'')

challenge16Encrypt :: ByteString -> AES -> ByteString -> ByteString
challenge16Encrypt iv key input = encryptCBC iv key plaintext
  where
    plaintext =
      padPKCS7 . B.concat $
        [ "comment1=cooking%20MCs;userdata=",
          input,
          ";comment2=%20like%20a%20pound%20of%20bacon"
        ]

challenge16IsAdmin :: ByteString -> AES -> ByteString -> Bool
challenge16IsAdmin iv key cipher =
  case M.lookup "admin" m of
    Just "true" -> True
    _ -> False
  where
    m = fromRight M.empty . parseKeyValue ';' . C8.unpack $ decryptCBC iv key cipher

-- TODO appears to fail occasionally with some (iv, key)s
challenge16 :: Assertion
challenge16 = do
  iv <- randomByteString 16
  key <- initAES <$> randomByteString 16
  let cipher = challenge16Encrypt iv key "mknawxadminxtrue"
  let blockSize = 16
  let adminLen = B.length "admin"
  let mask =
        B.concat
          [ B.replicate (blockSize + adminLen) 0,
            B.pack [67],
            B.replicate adminLen 0,
            B.pack [69],
            B.replicate (B.length cipher - 28) 0
          ]
  let cipher' = byteStringXor cipher mask
  assertBool "Challenge 16" $ challenge16IsAdmin iv key cipher'

challenge17Encrypt :: IO (ByteString, AES, ByteString)
challenge17Encrypt = do
  iv <- randomByteString 16
  key <- initAES <$> randomByteString 16
  cipher <-
    encryptCBC iv key . padPKCS7
      <$> randomSample
        [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
          "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
          "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
          "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
          "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
          "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
          "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
          "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
          "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
          "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
        ]
  return (iv, key, cipher)

type PaddingOracle = ByteString -> Bool

challenge17Oracle :: ByteString -> AES -> PaddingOracle
challenge17Oracle = (((isJust . stripPKCS7) .) .) . decryptCBC

byteFlips :: Int -> ByteString -> [ByteString]
byteFlips k b = byteStringXor b . B.pack <$> xs
  where
    len = B.length b
    xs = [shiftBy k (i : P.replicate (len - 1) 0) | i <- [0 .. 255]]

decryptByte :: Int -> ByteString -> ByteString -> PaddingOracle -> ByteString
decryptByte k x c o = byteStringXor pad . P.head . P.filter condition $ cs
  where
    pad = if k > 0 then padPKCS7 $ B.replicate k 0 else B.replicate 16 16
    x' = byteStringXor x pad
    xs = if k == 0 then [x'] else [x', P.head . P.tail . byteFlips (k - 1) $ x']
    cs = [B.append a c | a <- P.concatMap (byteFlips k) xs]
    condition c' =
      if k == 15
        then -- For the first one, have to confirm that we do not have valid non-01 ending.
          o c' && o (P.head $ byteFlips (k - 1) c')
        else o c'

decryptBlock :: PaddingOracle -> ByteString -> ByteString
decryptBlock o b = P.foldl go (B.replicate 16 0) (P.reverse [0 .. 15])
  where
    go i k = decryptByte k i b o

challenge17 :: Assertion
challenge17 = do
  (iv, key, cipher) <- challenge17Encrypt
  let oracle = challenge17Oracle iv key
  let decrypted = byteStringXor cipher . B.concat $ decryptBlock oracle <$> (P.tail . chunksOfBS 16 $ cipher)
  assertEqual "Challenge 17" (B.drop 16 $ decryptCBC iv key cipher) decrypted

challenge18 :: Assertion
challenge18 = do
  let key = initAES . C8.pack $ "YELLOW SUBMARINE"
  let cipher = B64.decode "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
  let decrypted = decryptCTR LittleEndian key cipher
  assertEqual "Challenge 18" "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby " decrypted

challenge19Data :: IO [ByteString]
challenge19Data = do
  key <- initAES <$> randomByteString 16
  return $
    encryptCTR LittleEndian key . B64.decode
      <$> [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ==",
            "Q29taW5nIHdpdGggdml2aWQgZmFjZXM=",
            "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ==",
            "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4=",
            "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk",
            "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
            "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ=",
            "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
            "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU=",
            "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl",
            "VG8gcGxlYXNlIGEgY29tcGFuaW9u",
            "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA==",
            "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk=",
            "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg==",
            "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo=",
            "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=",
            "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA==",
            "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA==",
            "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA==",
            "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg==",
            "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw==",
            "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA==",
            "U2hlIHJvZGUgdG8gaGFycmllcnM/",
            "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w=",
            "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4=",
            "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ=",
            "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs=",
            "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA==",
            "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA==",
            "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4=",
            "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA==",
            "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu",
            "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc=",
            "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs",
            "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs=",
            "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0",
            "SW4gdGhlIGNhc3VhbCBjb21lZHk7",
            "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw=",
            "VHJhbnNmb3JtZWQgdXR0ZXJseTo=",
            "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
          ]

challenge19 :: Assertion
challenge19 = do
  ciphers <- challenge19Data
  let c = L.maximumBy (compare `on` B.length) ciphers
  -- Per suggestion, deciphered this semi-manually with the aid of the following:
  --
  -- cribDrag :: ByteString -> ByteString -> IO ()
  -- cribDrag guess cipher = mapM_ print $ do
  --   (i, w) <- zip [0 ..] $ windowsOf (B.length guess) cipher
  --   let xord = byteStringXor guess w
  --   if i == 0
  --     then return (i, xord)
  --     else mempty
  --   where
  --     windowsOf :: Int -> ByteString -> [ByteString]
  --     windowsOf k s
  --       | B.length s < k = []
  --       | otherwise = B.take k s : windowsOf k (B.tail s)

  let key = byteStringXor c "He, too, has been changed in his turn,"
  let plains = [byteStringXor x key | x <- ciphers]

  assertEqual "Challenge 19" "I have met them at close of day" (P.head plains)
  assertEqual "Challenge 19" "A terrible beauty is born." (P.last plains)

challenge20 :: Assertion
challenge20 = do
  key <- initAES <$> randomByteString 16
  ciphers <- fmap (encryptCTR LittleEndian key . B64.decode . C8.pack) . lines <$> readFile "static/20.txt"
  let len = P.minimum $ B.length <$> ciphers
  let transposed = transposeBytes len . B.concat $ B.take len <$> ciphers
  let solved = singleCharXorSolver <$> transposed
  let key' = C8.pack $ _char <$> solved
  let plains = byteStringXor key' <$> ciphers
  assertEqual "Challenge 20" "I'm rated \"R\"...this is a warning, ya better void / P" (P.head plains)
  assertEqual "Challenge 20" "And we outta here / Yo, what happened to peace? / Pea" (P.last plains)


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
            TestCase challenge14,
            TestCase challenge15,
            TestCase challenge16,
            TestCase challenge17,
            TestCase challenge18,
            TestCase challenge19,
            TestCase challenge20
          ]
  _ <- runTestTT tests
  return ()
