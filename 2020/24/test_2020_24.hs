import Test.HUnit
import Puzzle_2020_24
import Parsing (parseInput)
import qualified Data.Set as S

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testTiles :: Test
testTiles = TestCase $ do
    (Just paths) <- parseInput pathsP "24/test_input.txt"
    10 @=? countTiles paths 


testExhibit :: Test
testExhibit = TestCase $ do
    (Just paths) <- parseInput pathsP "24/test_input.txt"
    let lobby = tileSet paths
    [10,15,12,25,14,23,28,41,37,49,37] @=? (map S.size $ take 11 (iterate update lobby))


testA = TestList
    [ TestLabel "Tiles" testTiles]

testB = TestList
    [ TestLabel "Exhibit" testExhibit]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()