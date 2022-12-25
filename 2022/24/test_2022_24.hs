import Test.HUnit
import Puzzle_2022_24
import Parsing (parseInput)
import Control.Monad.Reader
import Data.Sort (sort)
import Data.List (nub)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testPositionsSimple = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input_2.txt"
    [(1, 1), (4, 3)] @=? (runReader (positions 1) storm)
    [(1, 3)] @=? (runReader (positions 3) storm)
    [(1, 0), (3, 3)] @=? (runReader (positions 5) storm)


testFindGoalSimple = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input_2.txt"
    -- mapM_ print (blizzards storm)
    10 @=? findGoal storm


testPositions = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input.txt"
    let ps = [ (0, 1), (0, 2), (0, 2), (3, 4), (0, 4)
             , (1, 0), (1, 3), (1, 4)
             , (2, 1), (3, 1), (2, 4), (2, 3), (2, 0)
             , (3, 5), (2, 1), (0, 2), (2, 3), (2, 4), (3, 0)]
    (sort . nub $ ps) @=? (runReader (positions 1) storm)

testFindGoal = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input.txt"
    -- mapM_ print (blizzards storm)
    18 @=? findGoal storm


testRoundTrip = TestCase $ do
    (Just storm) <- parseInput stormP "24/test_input.txt"
    54 @=? roundTrip storm


testA = TestList
    [ TestLabel "Positions simple" testPositionsSimple
    , TestLabel "Find goal simple" testFindGoalSimple
    , TestLabel "Positions" testPositions
    , TestLabel "Find goal" testFindGoal]

testB = TestList
    [ TestLabel "Round trip" testRoundTrip]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()