import qualified Data.Massiv.Array as A

import Test.HUnit
import System.IO
import Puzzle_2018_18

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase (do
    forest <- parseInput "test_input_0.txt"
    A.Sz (10 A.:. 10) @=? (A.size forest))


testMagic = TestCase (do
    forest0 <- parseInput "test_input_0.txt"
    forest1 <- parseInput "test_input_1.txt"
    forest10 <- parseInput "test_input_10.txt"
    forest1 @=? (magic forest0)
    forest10 @=? (iterate magic forest0) !! 10)


testValue = TestCase (do
    forest10 <- parseInput "test_input_10.txt"
    1147 @=? value forest10)

testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Applying Magic" testMagic
    , TestLabel "Value" testValue]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()