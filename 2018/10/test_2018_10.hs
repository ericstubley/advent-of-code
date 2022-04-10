import Test.HUnit
import System.IO
import Puzzle_2018_10

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO ([Point], [Point])
parsedInput = do
    (points, velocities) <- parseInput "test_input.txt"
    return (points, velocities)

testParse = TestCase (do
    (ps, vs) <- parsedInput
    Point 9 1 @=? head ps
    Point 0 2 @=? head vs
    31 @=? length ps)


testTopLeft = TestCase (do
    (ps, vs) <- parsedInput
    Point (-6) (-4) @=? tl ps)


testGridSize = TestCase (do
    (ps, vs) <- parsedInput
    let (w0, h0) = dimensions ps
    let (w1, h1) = dimensions $ advance vs ps
    let (w2, h2) = dimensions $ iterate (advance vs) ps !! 2
    let (w3, h3) = dimensions $ iterate (advance vs) ps !! 3
    (22, 16) @=? (w0, h0) 
    (18, 12) @=? (w1, h1)
    (14, 10) @=? (w2, h2)
    (10, 8) @=? (w3, h3))


testA = TestList
    [ TestLabel "Parse" testParse
    , TestLabel "Top Left" testTopLeft
    , TestLabel "Grid Size" testGridSize]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()