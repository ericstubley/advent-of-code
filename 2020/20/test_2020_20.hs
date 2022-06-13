import Test.HUnit
import Puzzle_2020_20
import Parsing (parseInput)
import qualified Data.Map.Strict as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testAlign :: Test
testAlign = TestCase $ do
    (Just tiles) <- parseInput tilesP "20/test_input.txt"
    let counts = M.fromList [(1951, 2), (2311, 3), (3079, 2), (2729, 3), (1427, 4), (2473, 3), (2971, 2), (1489, 3), (1171, 2)]
    counts @=? matchCounts tiles


testLayout :: Test
testLayout = TestCase $ do
    (Just tiles) <- parseInput tilesP "20/test_input.txt"
    let a = assign tiles
    print a
    print "Done assigning"
    let o = orient a
    print o
    print "Done orienting"
    let t = (M.map trim) o
    print t
    print "Done trimming"
    let l = squash t
    print l
    print "Done squashing"



testRoughness :: Test
testRoughness = TestCase $ do
    (Just tiles) <- parseInput tilesP "20/test_input.txt"
    273 @=? roughness tiles


testA = TestList
    [ TestLabel "Alginment" testAlign]

testB = TestList
    [ TestLabel "Layout" testLayout
    , TestLabel "Roughness" testRoughness]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()