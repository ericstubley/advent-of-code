import Test.HUnit
import Puzzle_2023_06
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testStrategies = TestCase $ do
    (Just races) <- parseInput racesP "06/test_input.txt"
    [4, 8, 9] @=? map (length . strategies) races


testBigRace = TestCase $ do
    (Just races) <- parseInput racesP "06/test_input.txt"
    let bigRace = compress races
    71503 @=? (length . strategies) bigRace


testA = TestList
    [ TestLabel "Strategy counts" testStrategies]

testB = TestList
    [ TestLabel "Big race" testBigRace]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()