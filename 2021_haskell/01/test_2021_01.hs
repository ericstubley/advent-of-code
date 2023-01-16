import Test.HUnit
import Puzzle_2021_01
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testIncreases = TestCase $ do
    (Just depths) <- parseInput depthsP "01/test_input.txt"
    7 @=? increases depths 


testWindow3 = TestCase $ do
    (Just depths) <- parseInput depthsP "01/test_input.txt"
    [607, 618, 618, 617, 647, 716, 769, 792] @=? window3 depths


testA = TestList
    [ TestLabel "Increases" testIncreases]

testB = TestList
    [ TestLabel "Window3" testWindow3]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()