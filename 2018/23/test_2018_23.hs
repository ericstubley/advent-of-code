import Data.Massiv.Core.Index
import Test.HUnit
import System.IO
import Puzzle_2018_23

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO [Nanobot]
parsedInput = parseInput "test_input.txt"


parsedInput2 :: IO [Nanobot]
parsedInput2 = parseInput "test_input_2.txt"

testInRange = TestCase (do
    nanobots <- parsedInput
    7 @=? inRangeOfMax nanobots)

testBestPos = TestCase (do
    nanobots <- parsedInput2
    (12 :> 12 :. 12) @=? boxSearch nanobots (Octant (-32 :> -32 :. -32) (31:>31:.31)))

testNoBotsBox = TestCase (do
    (-10:> -10:. -10) @=? boxSearch [] (Octant (-20:> -20:. -20) (-10:> -10:. -10)))


testRandom = TestCase (do
    nanobots <- parsedInput2
    ix <- randomSearch nanobots
    (12 :> 12 :. 12) @=? ix)

testOctFilter = TestCase (do
    nanobots <- parsedInput2
    ix <- octFilter nanobots
    (12 :> 12 :. 12) @=? ix)


testOctize = TestCase (do
    let oct  = Octant (16 :> 16 :. 16) (32 :> 32 :. 32)
    let oct0 = Octant (16 :> 16 :. 16) (24 :> 24 :. 24)
    let oct1 = Octant (24 :> 16 :. 16) (32 :> 24 :. 24)
    let oct2 = Octant (16 :> 24 :. 16) (24 :> 32 :. 24)
    let oct3 = Octant (16 :> 16 :. 24) (24 :> 24 :. 32)
    let oct4 = Octant (24 :> 24 :. 16) (32 :> 32 :. 24)
    let oct5 = Octant (24 :> 16 :. 24) (32 :> 24 :. 32)
    let oct6 = Octant (16 :> 24 :. 24) (24 :> 32 :. 32)
    let oct7 = Octant (24 :> 24 :. 24) (32 :> 32 :. 32)
    [oct0, oct1, oct2, oct3, oct4, oct5, oct6, oct7] @=? octize oct)


testOctRange = TestCase (do
    bots <- parsedInput2
    let testBot = take 1 bots
    let octs = octize (Octant (-32 :> -32 :. -32) (32:>32:.32))
    [0,0,0,0,0,0,0,1] @=? map (numInRange testBot) octs
    )


testOctQueuer = TestCase (do
    nanobots <- parsedInput2
    ix <- octQueuer nanobots
    (12 :> 12 :. 12) @=? ix)




testA = TestList
    [ TestLabel "In Range" testInRange]

testB = TestList
    [ TestLabel "Best Pos?" testBestPos
    , TestLabel "No Bots Box Search" testNoBotsBox
    , TestLabel "Random Search YOLO" testRandom
    , TestLabel "Octizing" testOctize
    , TestLabel "Oct In Range" testOctRange
    , TestLabel "Oct Filter Search" testOctFilter
    , TestLabel "Oct Queuer" testOctQueuer]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()