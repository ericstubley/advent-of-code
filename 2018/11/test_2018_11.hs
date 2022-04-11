import Test.HUnit
import System.IO
import Puzzle_2018_11

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testPowerLevel = TestCase (do
    4   @=? powerLevel 8 (3, 5)
    -5  @=? powerLevel 57 (122, 79)
    0   @=? powerLevel 39 (217, 196)
    4   @=? powerLevel 71 (101, 153))


testBestRegion = TestCase (do
    (29, (33,45)) @=? bestRegion 18 300
    (30, (21,61)) @=? bestRegion 42 300)

testSmall = TestCase (do
    (10, ((10, 10), 10)) @=? bestRegionVariable 10 100)


testBestVariableRegion = TestCase (do
    (113, ((90, 269), 16)) @=? bestRegionVariable 18 300
    (119, ((232, 251), 12)) @=? bestRegionVariable 42 300)



testA = TestList
    [ TestLabel "Power Level" testPowerLevel
    , TestLabel "Best Region" testBestRegion]

testB = TestList
    [ TestLabel "Small test" testSmall
    , TestLabel "Best Variable Region" testBestVariableRegion]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()