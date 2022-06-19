import Data.Bit
import Data.Vector as V
import Data.Vector.Unboxed as UV
import Lib.Crypto
import Lib.Util
import Test.HUnit
import Prelude
import qualified Prelude as P

charToBit c
  | c == '0' = Bit False
  | c == '1' = Bit True
  | otherwise = error "expect char of 0 or 1"

test_intToBitVec :: Assertion
test_intToBitVec = do
  -- TODO could parametrize `chunks`, `perChunk`
  let expected = UV.fromList $ P.map charToBit "11001"
  assertEqual "chunkBy" expected (intToBitVec 25)

test_pad :: Assertion
test_pad = do
  -- TODO could parametrize `chunks`, `perChunk`
  let input = UV.fromList $ P.map charToBit "1111"
  let expected = UV.fromList $ P.map charToBit "00001111"
  assertEqual "chunkBy" expected (pad 8 input)

test_chunkBy :: Assertion
test_chunkBy = do
  -- TODO could parametrize `chunks`, `perChunk`
  let chunks = 4
  let perChunk = 3
  let input = UV.fromList [i | i <- [0 .. chunks * perChunk - 1]]
  let expected = V.fromList [UV.fromList [i * chunks + j :: Int | j <- [0 .. chunks - 1]] | i <- [0 .. perChunk - 1]]
  assertEqual "chunkBy" expected (chunkBy chunks input)

test_hexDecode = do
  let hexString = "a1b2c30"
  let expected = UV.fromList $ P.map charToBit "1010000110110010110000110000"
  assertEqual "hexDecode" expected (hexDecode hexString)

test_byteEncode :: Assertion
test_byteEncode = do
  let input = UV.fromList $ P.map charToBit "011001100110111101101111011000100110000101110010"
  let expected = "foobar"
  assertEqual "chunkBy" expected (byteEncode input)

test_base64Decode :: Assertion
test_base64Decode = do
  let input = "eXVjaw=="
  let expected = "yuck"
  assertEqual "base64Decode" expected (byteEncode $ base64Decode input)

test_hammingDistance :: Assertion
test_hammingDistance = do
  let a = byteDecode "this is a test"
  let b = byteDecode "wokka wokka!!!"
  assertEqual "hammingDistance" 37 (hammingDistance a b)

test_transposeBytes :: Assertion
test_transposeBytes = do
  let input = byteDecode "foobar"
  let expected = "fboaor"
  assertEqual "transposeBytes" expected (byteEncode . UV.concat . V.toList . transposeBytes 3 $ input)
  let input = byteDecode "deadbeef"
  let expected = "dbeeaedf"
  assertEqual "transposeBytes" expected (byteEncode . UV.concat . V.toList . transposeBytes 4 $ input)

main :: IO ()
main = do
  let tests =
        TestList
          [ TestCase test_intToBitVec,
            TestCase test_pad,
            TestCase test_chunkBy,
            TestCase test_hexDecode,
            TestCase test_byteEncode,
            TestCase test_base64Decode,
            TestCase test_hammingDistance,
            TestCase test_transposeBytes
          ]
  runTestTT tests
  return ()
