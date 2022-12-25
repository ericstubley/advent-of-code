import Test.HUnit
import Puzzle_2022_25
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

-- decimalExamples :: [Int]
-- decimalExamples = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022, 12345
--                   , 314159265, 1747, 906, 198, 11, 201, 31, 1257, 32, 353
--                   , 107, 7, 3, 37]
-- 
-- 
-- snafuExamples :: [String]
-- snafuExamples = [ "1", "2", "1=", "1-", "10", "11", "12", "2=", "2-", "20"
--                 , "1=0", "1-0", "1=11-2", "1-0---0", "1121-1110-1=0", "1=-0-2"
--                 , "12111", "2=0=", "21", "2=01", "111", "20012", "112", "1=-1="
--                 , "1-12", "12", "1=", "122"]
-- 
-- 
-- testSnafu = TestCase $ do
--     snafuExamples @=? map snafu decimalExamples
-- 
-- 
-- testUnfuck = TestCase $ do
--     decimalExamples @=? map unfuck snafuExamples


testSum = TestCase $ do
    (Just snafus) <- parseInput snafusP "25/test_input.txt"
    "2=-1=0" @=? (printer $ sum snafus)


testA = TestList
    -- [ TestLabel "Snafu" testSnafu
    -- , TestLabel "Unfuck" testUnfuck
    [ TestLabel "Sum" testSum]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()