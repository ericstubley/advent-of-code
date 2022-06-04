import Test.HUnit
import Puzzle_2020_07
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testParsing :: Test
testParsing = TestCase $ do
    (Just rules) <- parseInput rulesP "07/test_input.txt"
    9 @=? length rules
    [2, 2, 1, 2, 2, 2, 2, 0, 0] @=? map (length . snd) rules 



testComponent :: Test
testComponent = TestCase $ do
    (Just graph) <- parseInput graphP "07/test_input.txt"
    4 @=? containers graph (Bag "shiny" "gold") 


testContained :: Test
testContained = TestCase $ do
    (Just rules) <- parseInput rulesP "07/test_input.txt"
    (Just rules2) <- parseInput rulesP "07/test_input_2.txt"
    32 @=? bagsContained rules (Bag "shiny" "gold")
    126 @=? bagsContained rules2 (Bag "shiny" "gold")


testA = TestList
    [ TestLabel "Parsing" testParsing
    , TestLabel "Component Size" testComponent]

testB = TestList
    [ TestLabel "Bags Contained" testContained]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()