import Test.HUnit
import Puzzle_2020_15

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testDepth :: Test
testDepth = TestCase $ do
    [0,3,3,1,0,4,0] @=? (map (game [0,3,6]) [4..10])



testGame :: Test
testGame = TestCase $ do
    436  @=? game [0,3,6] 2020
    1    @=? game [1,3,2] 2020
    10   @=? game [2,1,3] 2020
    27   @=? game [1,2,3] 2020
    78   @=? game [2,3,1] 2020
    438  @=? game [3,2,1] 2020
    1836 @=? game [3,1,2] 2020




testA = TestList
    [ TestLabel "Test in depth" testDepth
    , TestLabel "Test Game" testGame]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()