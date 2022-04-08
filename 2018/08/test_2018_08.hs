import Test.HUnit
import System.IO
import Puzzle_2018_08

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
parsedInput :: IO [Int]
parsedInput = do
   tree <- parseInput "test_input.txt"
   return tree 


testMetadataSum = TestCase (do
    tree <- parsedInput
    138 @=? metadataSum tree)


testValue = TestCase (do
    tree <- parsedInput
    let (val, _) = value tree
    66 @=? val)


testA = TestList
    [ TestLabel "MD Sum" testMetadataSum]

testB = TestList
    [ TestLabel "Value" testValue]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()