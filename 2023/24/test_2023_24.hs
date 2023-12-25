import Test.HUnit
import Puzzle_2023_24
import Parsing (parseInput)
import Utilities (combinations)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testCollisions = TestCase $ do
    (Just hail) <- parseInput hailstonesP "24/test_input.txt"
    2 @=? collisions 7 27 hail


testSolveB = TestCase $ do
    (Just hail) <- parseInput hailstonesP "24/test_input.txt"
    47 @=? solveB hail

testA = TestList
    [ TestLabel "Collisions xy" testCollisions]

testB = TestList
    [ TestLabel "Solve B" testSolveB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()