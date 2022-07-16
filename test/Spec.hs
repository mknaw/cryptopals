{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Crypto.Cipher.AES (initAES)
import Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Lib.Encode.Base64 as B64
import Lib.Encode.Hex as HEX
import Lib.Crypto
import Lib.Util
import Test.HUnit
import Prelude

test_HEXdecode :: Assertion
test_HEXdecode = do
  assertEqual "HEX.decode" "foobar" (HEX.decode "666f6f626172")

test_HEXencode :: Assertion
test_HEXencode = do
  assertEqual "HEX.encode" "666f6f626172" (HEX.encode "foobar")

test_B64decode :: Assertion
test_B64decode = do
  let input = "eXVjaw=="
  let expected = "yuck"
  assertEqual "B64.decode" expected (B64.decode input)

test_hammingDistance :: Assertion
test_hammingDistance = do
  assertEqual "hammingDistance" 37 (hammingDistance "this is a test" "wokka wokka!!!")

test_manualCBC :: Assertion
test_manualCBC = do
  let plain = "lorem ipsum dolor sit amet, cons"
  let iv = "deadbeefdeadbeef"
  let key = initAES . C8.pack $ "yellow submarine"
  let cipher = encryptCBCManual iv key plain
  let decrypted = decryptCBCManual iv key cipher
  assertEqual "manualCBC" plain decrypted

test_commonPrefix :: Assertion
test_commonPrefix = do
  let a = "foobarbaz"
  let b = "foobarquux"
  let c = "barbazquux"
  assertEqual "commonPrefix" "foobar" (commonPrefix a b)
  assertEqual "commonPrefix" B.empty (commonPrefix a c)

main :: IO ()
main = do
  let tests =
        TestList
          [ TestCase test_HEXdecode,
            TestCase test_HEXencode,
            TestCase test_B64decode,
            TestCase test_hammingDistance,
            TestCase test_manualCBC,
            TestCase test_commonPrefix
          ]
  _ <- runTestTT tests
  return ()
