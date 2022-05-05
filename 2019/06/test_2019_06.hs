import Test.HUnit
import Puzzle_2019_06
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testOrbitCount = TestCase $ do
    (Just orrery) <- parseInput orbitsP "06/test_input.txt"
    42 @=? orbitSum orrery


testTransfers = TestCase $ do
    (Just orrery) <- parseInput orbitsP "06/test_input_2.txt"
    4 @=? transfers orrery you santa

testA = TestList
    [ TestLabel "Orbit Count" testOrbitCount]

testB = TestList
    [ TestLabel "Transfers" testTransfers]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()