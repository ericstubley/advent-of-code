import Test.HUnit
import Puzzle_2019_20
import Parsing
import Grid
import qualified Data.Map.Strict as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testPortals :: Test
testPortals = TestCase $ do
    (Just donut1) <- parseInput donutP "20/test_input_1.txt"
    -- (Just donut2) <- parseInput donutP "20/test_input_2.txt"
    let portals = M.fromList [ (2 :. 9,"AA")
                             , (6 :. 9,"BC")
                             , (8 :. 2,"BC")
                             , (10 :. 6,"DE")
                             , (12 :. 11,"FG")
                             , (13 :. 2,"DE")
                             , (15 :. 2,"FG")
                             , (16 :. 13,"ZZ")]
    portals @=? portalMap donut1
    -- M.empty @=? portalMap donut2


testShortest :: Test
testShortest = TestCase $ do
    (Just donut1) <- parseInput donutP "20/test_input_1.txt"
    (Just donut2) <- parseInput donutP "20/test_input_2.txt"
    23 @=? alphabetTour donut1
    58 @=? alphabetTour donut2


testShortestRecursive :: Test
testShortestRecursive = TestCase $ do
    (Just donut1) <- parseInput donutP "20/test_input_1.txt"
    (Just donut3) <- parseInput donutP "20/test_input_3.txt"
    26  @=? alphabetTourRecursive donut1
    396 @=? alphabetTourRecursive donut3



testA = TestList
    [ TestLabel "Portal Finder" testPortals
    , TestLabel "Shortest Paths" testShortest]

testB = TestList
    [ TestLabel "Shortest Paths Recursive" testShortestRecursive]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()