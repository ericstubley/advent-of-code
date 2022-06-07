import Test.HUnit
import Puzzle_2020_11
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testStable :: Test
testStable = TestCase $ do
    (Just initial) <- parseInput seatsP "11/test_input.txt"
    (Just stable) <- parseInput seatsP "11/test_input_stable.txt"
    stable @=? stableState initial
    37 @=? countOccupied stable


testStable' :: Test
testStable' = TestCase $ do
    (Just initial) <- parseInput seatsP "11/test_input.txt"
    (Just stable) <- parseInput seatsP "11/test_input_stable_2.txt"
    stable @=? stableState' initial
    26 @=? countOccupied stable

testA = TestList
    [ TestLabel "Stable Seating" testStable]

testB = TestList
    [ TestLabel "Stable Seating 2" testStable']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()