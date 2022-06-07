import Test.HUnit
import Puzzle_2020_12
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testNavigation :: Test
testNavigation = TestCase $ do
    (Just commands) <- parseInput commandsP "12/test_input.txt"
    (17, -8) @=? finalPosition commands 


testRelative :: Test
testRelative = TestCase $ do
    (Just commands) <- parseInput commandsP "12/test_input.txt"
    (214, -72) @=? finalRelative commands

testA = TestList
    [ TestLabel "Navigation" testNavigation]

testB = TestList
    [ TestLabel "Relative" testRelative]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()