module Main where

import Control.Monad
import Data.Bit (zipBits)
import Data.Bits (xor)
import Data.Monoid
import Lib.Crypto
import Test.HUnit

test1 =
  TestCase
    ( do
        let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        let encoded = base64Encode . hexDecode $ input
        let output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        assertEqual "base64 encoded" encoded output
    )

test2 = TestCase (do
    let a = hexDecode "1c0111001f010100061a024b53535009181c"
    let b = hexDecode "686974207468652062756c6c277320657965"
    let xord = hexEncode $ zipBits xor a b
    let output = "746865206b696420646f6e277420706c6179"
    assertEqual "XOR'd" xord output
    )

main :: IO ()
main = do
  let tests = TestList [test1, test2]
  runTestTT tests
  return ()
