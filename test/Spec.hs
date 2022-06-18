import Data.Bit
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Lib.Util (chunkBy)
import Prelude
import qualified Prelude as P
import Test.HUnit

charToBit c
  | c == '0' = Bit False
  | c == '1' = Bit True
  | otherwise = error "expect char of 0 or 1"

test_chunkBy =
  TestCase
    ( do
        -- TODO could parametrize `chunks`, `perChunk`
        let chunks = 4
        let perChunk = 3
        let input = UV.fromList [i | i <- [0..chunks * perChunk - 1]]
        let output = V.fromList [UV.fromList [i * chunks + j :: Int | j <- [0..chunks - 1]] | i <- [0..perChunk - 1]]
        assertEqual "chunkBy" output (chunkBy chunks input)
    )

test_hexDecode = do
  let hexString = "a1b2c30"
  let output = UV.fromList $ P.map charToBit "1010000110110010110000110000"
  assertEqual "hexDecode" output (hexDecode hexString)

test_byteEncode =
  TestCase
    ( do
        let input = UV.fromList $ P.map charToBit "011001100110111101101111011000100110000101110010"
        let output = "foobar"
        assertEqual "chunkBy" output (byteEncode input)
    )

main :: IO ()
main = do
  let tests =
        TestList
          [ test_chunkBy,
            TestCase test_hexDecode,
            test_byteEncode
          ]
  runTestTT tests
  return ()
