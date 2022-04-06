import Test.HUnit
import qualified Data.Set as Set
import System.IO
import Puzzle_2018_06

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO [Point]
parsedInput = do
    points <- parseInput "test_input.txt"
    return points


testFindBoundary = TestCase (do
    points <- parsedInput
    let (minB, maxB) = findBoundary points
    Point 1 1 @=? minB
    Point 8 9 @=? maxB)


testBoundaryPoints = TestCase (do
    points <- parsedInput
    let bps = findBoundaryPoints points
    Set.fromList [Point 1 1, Point 1 6, Point 8 3, Point 8 9] @=? bps)


testFiniteAreaPoints = TestCase (do
    points <- parsedInput
    let fps = finiteAreaPoints points
    Set.fromList [Point 3 4, Point 5 5] @=? fps)


testMaxFiniteArea = TestCase (do
    points <- parsedInput
    let mfa = maxFiniteArea points
    17 @=? mfa)


testTotalDistance = TestCase (do
    points <- parsedInput
    let td = totalDistance (Point 4 3) points
    30 @=? td)


testSafePoints = TestCase (do
    points <- parsedInput
    let numSafePoints = length $ safePoints 32 points
    16 @=? numSafePoints) 


testA = TestList
    [ TestLabel "Identify Boundary" testFindBoundary
    , TestLabel "Identify Boundary Points" testBoundaryPoints
    , TestLabel "Identify Finite Area Points" testFiniteAreaPoints
    , TestLabel "Maximum Finite Area" testMaxFiniteArea]

testB = TestList
    [ TestLabel "Total Distance" testTotalDistance
    , TestLabel "Safe Point Count" testSafePoints]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()