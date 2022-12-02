import Test.HUnit
import Puzzle_2022_02
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testScore = TestCase $ do
    (Just guide) <- parseInput guideP "02/test_input.txt" 
    let score = scoreGuide guide
    15 @=? score


testAdvancedScore = TestCase $ do
    (Just guide) <- parseInput advancedGuideP "02/test_input.txt"
    let score = advancedScoreGuide guide
    12 @=? score 


testA = TestList
    [ TestLabel "Guide Scoring" testScore]

testB = TestList
    [ TestLabel "Advanced Guide Scoring" testAdvancedScore]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()