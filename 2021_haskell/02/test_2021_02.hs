import Test.HUnit
import Puzzle_2021_02
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testNavigate = TestCase $ do
    (Just course) <- parseInput courseP "02/test_input.txt"
    (15, 10) @=? navigate course


testNavigate' = TestCase $ do
    (Just course) <- parseInput courseP "02/test_input.txt"
    (15, 60) @=? navigate' course


testA = TestList
    [ TestLabel "Navigate" testNavigate]

testB = TestList
    [ TestLabel "Navigate'" testNavigate']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()