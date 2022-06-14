import Test.HUnit
import Puzzle_2020_22
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testCombat :: Test
testCombat = TestCase $ do
    (Just (deck1, deck2)) <- parseInput decksP "22/test_input.txt"
    let final = combat deck1 deck2
    306 @=? score final 


testRecursive :: Test
testRecursive = TestCase $ do
    (Just (deck1, deck2)) <- parseInput decksP "22/test_input.txt"
    let final = recursiveCombat deck1 deck2
    291 @=? score final



testA = TestList
    [ TestLabel "Combat" testCombat]

testB = TestList
    [ TestLabel "Recursive Combat" testRecursive]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()