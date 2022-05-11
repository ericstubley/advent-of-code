import Test.HUnit
import Puzzle_2019_13

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testBlockCount :: Test
testBlockCount = TestCase $ do
    0 @=? blockCount [1,2,3,6,5,4]

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