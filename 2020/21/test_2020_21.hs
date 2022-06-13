import Test.HUnit
import Puzzle_2020_21
import Parsing (parseInput)
import qualified Data.Set as S

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testCoarse :: Test
testCoarse = TestCase $ do
    (Just labels) <- parseInput labelsP "21/test_input.txt"
    ["kfcds", "nhms", "sbzzf", "trh"] @=? (S.toList $ safeFoods labels)

testCount :: Test
testCount = TestCase $ do
    (Just labels) <- parseInput labelsP "21/test_input.txt"
    5 @=? countSafe labels


testA = TestList
    [ TestLabel "Coarse Filter" testCoarse]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()