import Test.HUnit
import Puzzle_2022_03
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testOverlap = TestCase $ do
    (Just bags) <- parseInput bagsP "03/test_input.txt"
    let os = map overlap bags
    "pLPvts" @=? os


testPriority = TestCase $ do
    let ps = map priority "pLPvts"
    [16, 38, 42, 22, 20, 19] @=? ps


testBadges = TestCase $ do
    (Just bags) <- parseInput bagsP "03/test_input.txt"
    let bs = badges bags
    "rZ" @=? bs

testA = TestList
    [ TestLabel "Overlap" testOverlap
    , TestLabel "Priority" testPriority]

testB = TestList
    [ TestLabel "Badges" testBadges]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()