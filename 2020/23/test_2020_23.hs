import Test.HUnit
import Puzzle_2020_23

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testCups :: Test
testCups = TestCase $ do
    92658374 @=? (after1 $ play 389125467 10)
    67384529 @=? (after1 $ play 389125467 100)


testBig :: Test
testBig = TestCase $ do
    149245887792 @=? (nbhd $ playBig 389125467 10000000) 


testA = TestList
    [ TestLabel "Cup Shuffle" testCups]

testB = TestList
    [ TestLabel "Big Test" testBig]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()