
import Data.Massiv.Array (Ix2(..))

import Test.HUnit
import System.IO
import Puzzle_2018_22

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testGrid = TestCase (do
    let eg = erosionGrid 510 (10:.10) (10:.10)
    printGrid caveRepr eg
    putStrLn "Does this grid look okay?"
    confirm <- getLine
    "y" @=? confirm)

testRisk = TestCase (do
    let eg = erosionGrid 510 (10:.10) (10:.10)
    114 @=? risk eg)

testFastest = TestCase (do
    45 @=? fastestRoute 510 (10:.10) (20:.20)) 


testA = TestList
    [ TestLabel "Print Grid" testGrid
    , TestLabel "Total Risk" testRisk]

testB = TestList
    [ TestLabel "Fastest Route" testFastest]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()