import Test.HUnit
import Puzzle_2022_20
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testIntermediateLists = TestCase $ do
    (Just file) <- parseInput fileP "20/test_input.txt"
    [2, 1, -3, 3, -2, 0, 4] @=? intermediateList file 1
    [1, -3, 2, 3, -2, 0, 4] @=? intermediateList file 2
    [1, 2, 3, -2, -3, 0, 4] @=? intermediateList file 3
    [1, 2, -2, -3, 0, 3, 4] @=? intermediateList file 4
    [-2, 1, 2, -3, 0, 3, 4] @=? intermediateList file 5
    [-2, 1, 2, -3, 0, 3, 4] @=? intermediateList file 6
    [-2, 1, 2, -3, 4, 0, 3] @=? intermediateList file 7


testFinalList = TestCase $ do
    (Just file) <- parseInput fileP "20/test_input.txt"
    [0, 3, -2, 1, 2, -3, 4] @=? finalList file


testGroveCoordinateSum = TestCase $ do
    (Just file) <- parseInput fileP "20/test_input.txt"
    3 @=? groveCoordinateSum file


testMixes = TestCase $ do
    (Just file) <- parseInput fileP "20/test_input.txt"
    [0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153] @=? finalList' file 1
    [0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153] @=? finalList' file 2
    [0, 811589153, 2434767459, 3246356612, 1623178306, -1623178306, -2434767459] @=? finalList' file 3
    [0, 1623178306, -2434767459, 811589153, 2434767459, 3246356612, -1623178306] @=? finalList' file 4
    [0, 811589153, -1623178306, 1623178306, -2434767459, 3246356612, 2434767459] @=? finalList' file 5
    [0, 811589153, -1623178306, 3246356612, -2434767459, 1623178306, 2434767459] @=? finalList' file 6
    [0, -2434767459, 2434767459, 1623178306, -1623178306, 811589153, 3246356612] @=? finalList' file 7
    [0, 1623178306, 3246356612, 811589153, -2434767459, 2434767459, -1623178306] @=? finalList' file 8
    [0, 811589153, 1623178306, -2434767459, 3246356612, 2434767459, -1623178306] @=? finalList' file 9
    [0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153] @=? finalList' file 10


testGroveCoordinateSum' = TestCase $ do
    (Just file) <- parseInput fileP "20/test_input.txt"
    1623178306 @=? groveCoordinateSum' file





testA = TestList
    [ testIntermediateLists
    , testFinalList
    , testGroveCoordinateSum]

testB = TestList
    [ testMixes
    , testGroveCoordinateSum']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()