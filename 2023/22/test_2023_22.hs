import Test.HUnit
import Puzzle_2023_22
import Parsing (parseInput)
import Control.Monad.State
import qualified Data.Set as S

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testDebug = TestCase $ do
    (Just bricks) <- parseInput bricksP "22/test_input.txt"
    return ()


testJenga = TestCase $ do
    (Just bricks) <- parseInput bricksP "22/test_input.txt"
    5 @=? jenga bricks

testSolveB = TestCase $ do
    (Just bricks) <- parseInput bricksP "22/test_input.txt"
    7 @=? solveB bricks

testA = TestList
    [ TestLabel "Debug" testDebug
    , TestLabel "Jenga" testJenga]

testB = TestList
    [ TestLabel "Solve B" testSolveB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()