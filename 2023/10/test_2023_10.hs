import Test.HUnit
import Puzzle_2023_10
import Parsing (parseInput)
import Grid (printMap)
import qualified Data.Massiv.Array as A

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testParse = TestCase $ do
    (Just env) <- parseInput tilesP "10/test_input.txt"
    printMap . map (map printer) . A.toLists2 . _tiles $ env


testFurthest = TestCase $ do
    (Just env) <- parseInput tilesP "10/test_input.txt"
    4 @=? furthest env
    (Just env) <- parseInput tilesP "10/test_input_2.txt"
    8 @=? furthest env


testEnclosed = TestCase $ do
    (Just env) <- parseInput tilesP "10/test_input.txt"
    1 @=? enclosed env
    (Just env) <- parseInput tilesP "10/test_input_7.txt"
    2 @=? enclosed env
    (Just env) <- parseInput tilesP "10/test_input_3.txt"
    4 @=? enclosed env
    (Just env) <- parseInput tilesP "10/test_input_4.txt"
    4 @=? enclosed env
    (Just env) <- parseInput tilesP "10/test_input_5.txt"
    8 @=? enclosed env
    (Just env) <- parseInput tilesP "10/test_input_6.txt"
    10 @=? enclosed env


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Furthest" testFurthest]

testB = TestList
    [ TestLabel "Enclosed" testEnclosed]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()