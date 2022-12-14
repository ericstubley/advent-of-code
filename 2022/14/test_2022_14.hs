import Test.HUnit
import Puzzle_2022_14
import Parsing (parseInput)
import qualified Data.Map as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase $ do
    (Just cavern) <- parseInput cavernP "14/test_input.txt"
    20 @=? M.size cavern 


testAbyssVolume = TestCase $ do
    (Just cavern) <- parseInput cavernP "14/test_input.txt"
    24 @=? abyssVolume cavern


testFloorVolume = TestCase $ do
    (Just cavern) <- parseInput cavernP "14/test_input.txt"
    93 @=? floorVolume cavern


testA = TestList
    [ testParse
    , testAbyssVolume]

testB = TestList
    [ testFloorVolume]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()