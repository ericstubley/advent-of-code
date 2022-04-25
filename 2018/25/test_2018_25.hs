import Test.HUnit
import System.IO
import Puzzle_2018_25
import qualified Data.Set as S

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testConstellationCount = TestCase (do
    starsA <- parseInput "test_input_a.txt"
    starsB <- parseInput "test_input_b.txt"
    starsC <- parseInput "test_input_c.txt"
    starsD <- parseInput "test_input_d.txt"
    -- print $ constellations starsA
    2 @=? (S.size $ constellations starsA)
    4 @=? (S.size $ constellations starsB)
    3 @=? (S.size $ constellations starsC)
    8 @=? (S.size $ constellations starsD)
    )

testA = TestList
    [ TestLabel "Constellation Counts" testConstellationCount]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()