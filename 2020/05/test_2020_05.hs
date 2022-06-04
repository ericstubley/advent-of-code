import Test.HUnit
import Puzzle_2020_05
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testSeatID :: Test
testSeatID = TestCase $ do
    (Just seats) <- parseInput seatsP "05/test_input.txt"
    [567, 119, 820] @=? seats 

testA = TestList
    [ TestLabel "Seat IDs" testSeatID]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()