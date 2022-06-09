import Test.HUnit
import Puzzle_2020_17
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testSimulate :: Test
testSimulate = TestCase $ do
    (Just grid) <- parseInput gridP "17/test_input.txt"
    5 @=? (numberActive grid)
    11 @=? (numberActive $ step grid )
    112 @=? (numberActive $ (iterate step grid) !! 6)


testA = TestList
    [ TestLabel "Simulation" testSimulate]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()