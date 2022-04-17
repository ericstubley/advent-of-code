import qualified Data.Map.Strict as M
import Test.HUnit
import System.IO
import Puzzle_2018_17

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO GroundMap
parsedInput = parseInput "test_input.txt"


testParseSize = TestCase (do
    gm <- parsedInput
    34 @=? M.size gm)


testParse = TestCase (do
    gm <- parsedInput
    printGrid gm
    putStrLn "Does this grid look okay?"
    confirm <- getLine
    "y" @=? confirm)


testFall = TestCase (do
    gm <- parsedInput
    let filled = fall gm spring
    printGrid filled
    putStrLn "Does this grid look okay?"
    confirm <- getLine
    "y" @=? confirm)

testSmallGrid = TestCase (do
    gm <- parseInput "smaller_input.txt"
    printGrid gm
    putStrLn "Separator"
    let filled = fall gm spring
    printGrid filled
    True @=? True)



testWaterCount = TestCase (do
    clay <- parsedInput
    let nWater = countWater $ fall clay spring
    57 @=? nWater)


testStillWaterCount = TestCase (do
    clay <- parsedInput
    let n = countStillWater $ fall clay spring
    29 @=? n)
    

testA = TestList
    [ TestLabel "Parsing Size" testParseSize
    -- , TestLabel "Small Grid" testSmallGrid
    , TestLabel "Parsing and Printing" testParse
    , TestLabel "Printed Fill" testFall
    , TestLabel "Water Count" testWaterCount]

testB = TestList
    [ TestLabel "Still Water" testStillWaterCount]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()