import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import Test.HUnit
import System.IO
import Puzzle_2018_12

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO (Rules, Planters)
parsedInput = do
    (rules, pots) <- parseInput "test_input.txt"
    return (rules, pots)

testParse = TestCase (do
    (rules, pots) <- parsedInput
    IM.fromList (zip [0..] "#..#.#..##......###...###") @=? pots
    32 @=? M.size rules
    '.' @=? rules M.! "....."
    '#' @=? rules M.! "...##")


testEvolve = TestCase (do
    (rules, pots) <- parsedInput
    IM.fromList (zip [(-2)..] "..#...#....#.....#..#..#..#..") @=? evolve rules pots)


testIndexSum = TestCase (do
    (rules, pots) <- parsedInput
    325 @=? (indexSum $ (iterate (evolve rules) pots) !! 20))



testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Evolution" testEvolve
    , TestLabel "Index Sum" testIndexSum]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()