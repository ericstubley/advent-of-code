import Test.HUnit
import Puzzle_2023_02
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testParsing = TestCase $ do
    (Just games) <- parseInput gamesP "02/test_input.txt"
    Game 1 [Cubes 4 0 3, Cubes 1 2 6, Cubes 0 2 0] @=? (games !! 0)


testFeasible = TestCase $ do
    (Just games) <- parseInput gamesP "02/test_input.txt"
    let limit = Cubes 12 13 14
    [True, True, False, False, True] @=? map (feasible limit) games


testGameSum = TestCase $ do
    (Just games) <- parseInput gamesP "02/test_input.txt"
    let limit = Cubes 12 13 14
    8 @=? gameSum limit games


testEnvelopes = TestCase $ do
    (Just games) <- parseInput gamesP "02/test_input.txt"
    let bags = map smallestBag games
    Cubes 4 2 6 @=? bags !! 0
    Cubes 1 3 4 @=? bags !! 1
    Cubes 20 13 6 @=? bags !! 2 
    Cubes 14 3 15 @=? bags !! 3
    Cubes 6 3 2 @=? bags !! 4


testPowerSum = TestCase $ do
    (Just games) <- parseInput gamesP "02/test_input.txt"
    2286 @=? powerSum games 


testA = TestList
    [ TestLabel "Parsing A" testParsing
    , TestLabel "Feasible function" testFeasible
    , TestLabel "GameSum function" testGameSum]

testB = TestList
    [ TestLabel "Envelopes" testEnvelopes
    , TestLabel "PowerSum function" testPowerSum]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()