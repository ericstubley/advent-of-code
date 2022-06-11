import Test.HUnit
import Puzzle_2020_19
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testMatch :: Test
testMatch = TestCase $ do
    (Just (rules, messages)) <- parseInput satelliteP "19/test_input.txt"
    [True, False, True, False, False] @=? map (validate rules) messages


testFancy :: Test
testFancy = TestCase $ do
    (Just (rules, messages)) <- parseInput satelliteP "19/test_input_2.txt"
    let rules' = fancify rules

    3 @=? (length $ filter (validate rules) messages)
    12 @=? (length $ filter (validate rules') messages)



testA = TestList
    [ TestLabel "Match" testMatch]

testB = TestList
    [ TestLabel "Fancy" testFancy]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()