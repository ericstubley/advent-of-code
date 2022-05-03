import Test.HUnit
import Puzzle_2019_03
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testIntersections :: Test
testIntersections = TestCase $ do
    Just (a, b) <- parseInput wiresP "test_input_1.txt"
    [(3, 3), (6, 5)] @=? intersections a b


testDists :: Test
testDists = TestCase $ do
    Just (a1, b1) <- parseInput wiresP "test_input_1.txt"
    Just (a2, b2) <- parseInput wiresP "test_input_2.txt"
    Just (a3, b3) <- parseInput wiresP "test_input_3.txt"
    6 @=? closestIntersection a1 b1
    159 @=? closestIntersection a2 b2
    135 @=? closestIntersection a3 b3


testDelay :: Test
testDelay = TestCase $ do
    Just (a1, b1) <- parseInput wiresP "test_input_1.txt"
    Just (a2, b2) <- parseInput wiresP "test_input_2.txt"
    Just (a3, b3) <- parseInput wiresP "test_input_3.txt"
    30 @=? minimalDelay a1 b1
    610 @=? minimalDelay a2 b2
    410 @=? minimalDelay a3 b3


testA = TestList
    [ TestLabel "Intersections" testIntersections
    , TestLabel "Distances" testDists ]

testB = TestList
    [ TestLabel "Delay" testDelay]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()