import Test.HUnit
import Puzzle_2021_07
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testLinearMinimize = TestCase $ do
    (Just crabs) <- parseInput crabsP "07/test_input.txt"
    37 @=? linearMinimize crabs 


testQuadraticMinimize = TestCase $ do
    (Just crabs) <- parseInput crabsP "07/test_input.txt"
    168 @=? quadraticMinimize crabs 


testA = TestList
    [ TestLabel "Linear Minimization" testLinearMinimize]

testB = TestList
    [ TestLabel "Quadratic Minimization" testQuadraticMinimize]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()