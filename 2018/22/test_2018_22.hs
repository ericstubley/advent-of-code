import HUnit
import System.IO
import Puzzle_2018_22

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testA = TestList
    []

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()