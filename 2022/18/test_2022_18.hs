import Test.HUnit
import Puzzle_2022_18
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testSurfaceArea = TestCase $ do
    (Just cubes) <- parseInput cubesP "18/test_input.txt"
    64 @=? surfaceArea cubes 


testExteriorSurfaceArea = TestCase $ do
    (Just cubes) <- parseInput cubesP "18/test_input.txt"
    58 @=? exteriorSurfaceArea cubes


testA = TestList
    [ testSurfaceArea]

testB = TestList
    [ testExteriorSurfaceArea]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()