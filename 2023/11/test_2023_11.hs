import Test.HUnit
import Puzzle_2023_11
import Parsing (parseInput)
import Data.Massiv.Array (Ix2(..))
import Control.Monad.Reader

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testAnalyze = TestCase $ do
    (Just universe) <- parseInput universeP "11/test_input.txt"
    let rows = runReader weft universe
    let cols = runReader warp universe
    [3, 7] @=? rows
    [2, 5, 8] @=? cols
    9 @=? inflateDistance 2 rows cols ((5:.1), (9:. 4))
    15 @=? inflateDistance 2 rows cols ((0:.3), (8:. 7))
    17 @=? inflateDistance 2 rows cols ((2:.0), (6:. 9))
    5 @=? inflateDistance 2 rows cols ((9:.0), (9:. 4))
    374 @=? analyzeNow universe


testAnalyzeLater = TestCase $ do
    (Just universe) <- parseInput universeP "11/test_input.txt"
    1030 @=? analyze 10 universe
    8410 @=? analyze 100 universe

testA = TestList
    [ TestLabel "Analyze" testAnalyze]

testB = TestList
    [ TestLabel "Analyze later" testAnalyzeLater]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()