import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Test.HUnit
import System.IO
import Puzzle_2018_14

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testCook = TestCase (do
    let expected = Scoreboard 0 1 (S.fromList [3, 7, 1, 0])
    let cooked = newRecipe startingBoard
    expected @=? cooked)

testTenAfter = TestCase (do
    [5,1,5,8,9,1,6,7,7,9] @=? tenAfter 9
    [0,1,2,4,5,1,5,8,9,1] @=? tenAfter 5
    [9,2,5,1,0,7,1,0,8,5] @=? tenAfter 18
    [5,9,4,1,4,2,9,8,8,2] @=? tenAfter 2018)

testRecipesLeft = TestCase (do
    9    @=? recipesLeft [5,1,5,8,9,1,6,7,7,9]
    5    @=? recipesLeft [0,1,2,4,5,1,5,8,9,1]
    18   @=? recipesLeft [9,2,5,1,0,7,1,0,8,5]
    2018 @=? recipesLeft [5,9,4,1,4,2,9,8,8,2])


testA = TestList
    [ TestLabel "New Recipe" testCook
    , TestLabel "Ten After" testTenAfter]

testB = TestList
    [ TestLabel "Recipes Left" testRecipesLeft]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()