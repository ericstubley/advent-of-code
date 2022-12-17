import Test.HUnit
import Puzzle_2022_16
import Parsing (parseInput)
import qualified Data.Map.Strict as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testMaximumRelease = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/test_input.txt"
    1651 @=? maximumRelease adjacencies rates


testPath = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/test_input.txt"
    let graph = preprocess adjacencies rates
    print $ length $ paths graph 26


testScore = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/test_input.txt"
    let graph = preprocess adjacencies rates
    let path = ["DD", "BB", "JJ", "HH", "EE", "CC"]
    1651 @=? score graph 30 (map (renamer rates) path)


testBestPath = TestCase $ do
    (Just (adjacencies, rates)) <- parseInput graphP "16/input.txt"
    let graph = preprocess adjacencies rates
    print $ length $ paths graph 30
    print $ length $ paths graph 26
    1923 @=? bestPath graph 30


testA = TestList
    [ testMaximumRelease
    , testPath
    , testScore
    , testBestPath]

testB = TestList
    [ testCollaborativeRelease]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()