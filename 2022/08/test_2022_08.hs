import Test.HUnit
import Puzzle_2022_08
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testMarkVisible = TestCase $ do
    (Just forest) <- parseInput forestP "08/test_input.txt"
    let correct = [ [True, True, True, True, True]
                  , [True, True, True, False, True]
                  , [True, True, False, True, True]
                  , [True, False, True, False, True]
                  , [True, True, True, True, True]]
    correct @=? markVisible forest


testCountVisible = TestCase $ do
    (Just forest) <- parseInput forestP "08/test_input.txt"
    21 @=? countVisible forest


testViewingDistances = TestCase $ do
    [0, 1, 2, 3, 1] @=? views [3, 0, 3, 7, 3]
    [0, 1, 1, 1, 2] @=? views [2, 5, 5, 1, 2] 
    [0, 1, 1, 1, 1] @=? views [6, 5, 3, 3, 2]
    [0, 1, 2, 1, 4] @=? views [3, 3, 5, 4, 9]
    [0, 1, 1, 3, 1] @=? views [3, 5, 3, 9, 0]

    [0, 1, 2, 1, 1] @=? views [3, 2, 6, 3, 3]
    [0, 1, 1, 1, 2] @=? views [0, 5, 5, 3, 5]
    [0, 1, 1, 2, 1] @=? views [3, 5, 3, 5, 3]
    [0, 1, 2, 3, 4] @=? views [7, 1, 3, 4, 9]
    [0, 1, 1, 3, 1] @=? views [3, 2, 2, 9, 0]


testScenicScores = TestCase $ do
    (Just forest) <- parseInput forestP "08/test_input.txt"
    let ss = scenicScores forest
    4 @=? (ss !! 1) !! 2
    8 @=? (ss !! 3) !! 2


testMostScenic = TestCase $ do
    (Just forest) <- parseInput forestP "08/test_input.txt"
    8 @=? mostScenic forest


testA = TestList
    [ TestLabel "Mark Visible" testMarkVisible
    , TestLabel "Count Visible" testCountVisible]

testB = TestList
    [ TestLabel "Viewing Distances" testViewingDistances
    , TestLabel "Scenic Scores" testScenicScores
    , TestLabel "Most Scenic" testMostScenic]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()