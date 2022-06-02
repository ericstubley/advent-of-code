import Test.HUnit
import Puzzle_2020_03
import Parsing (parseInput)
import Data.Massiv.Array (Ix2(..))

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testTreesHit :: Test
testTreesHit = TestCase $ do
    (Just trees) <- parseInput treesP "03/test_input.txt"
    7 @=? treesHit (1:.3) trees 


testAverageHits :: Test
testAverageHits = TestCase $ do
    (Just trees) <- parseInput treesP "03/test_input.txt"
    336 @=? averageHits trees


testA = TestList
    [ TestLabel "Small test track" testTreesHit]

testB = TestList
    [ TestLabel "Average Hits" testAverageHits]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()