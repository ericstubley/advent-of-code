import Test.HUnit
import Puzzle_2023_18
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testAreaA = TestCase $ do
    (Just planA) <- parseInput planAP "18/test_input.txt"
    62 @=? area planA

testAreaB = TestCase $ do
    (Just planB) <- parseInput planBP "18/test_input.txt"
    952408144115 @=? area planB

testA = TestList
    [ TestLabel "AreaA" testAreaA]

testB = TestList
    [ TestLabel "AreaB" testAreaB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()