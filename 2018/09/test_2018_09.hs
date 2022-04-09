import Test.HUnit
import System.IO
import Puzzle_2018_09
import qualified Data.CircularList as CList

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testHighScore = TestCase (do
    let games = [(9, 25), (10, 1618), (13, 7999), (17, 1104), (21, 6111), (30, 5807)]
    let scores = map (\x -> highScore (fst x) (snd x)) games
    [32, 8317, 146373, 2764, 54718, 37305] @=? scores)


testBoards = TestCase (do
    let firstBoards = take 4 boards
    let expected =  [ CList.fromList [0]
                    , CList.fromList [1, 0]
                    , CList.fromList [2, 1, 0]
                    , CList.fromList [3, 0, 2, 1]]
    expected @=? firstBoards)

testA = TestList
    [ TestLabel "High Scores" testHighScore
    , TestLabel "First Boards" testBoards]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()