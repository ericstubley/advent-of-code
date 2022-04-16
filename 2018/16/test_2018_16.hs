
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import System.IO
import Puzzle_2018_16

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase (do
    (ss, _) <- parseInput "test_input.txt"
    let s = Sample (V.fromList [3, 2, 1, 1]) (V.fromList [3, 2, 2, 1]) (OP 9 (OPIO 2 1 2))
    [s] @=? ss)



testPossible = TestCase (do
    let s = Sample (V.fromList [3, 2, 1, 1]) (V.fromList [3, 2, 2, 1]) (OP 9 (OPIO 2 1 2))
    [ADDI, MULR, SETI] @=? possibleOPCodes s)



testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Possible OPCodes" testPossible]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()