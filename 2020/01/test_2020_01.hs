import Test.HUnit
import Puzzle_2020_01
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testExpenses :: Test
testExpenses = TestCase $ do
    (Just expenses) <- parseInput expensesP "01/test_input.txt"
    514579 @=? findMatch expenses 


testThreeExpenses :: Test
testThreeExpenses = TestCase $ do
    (Just expenses) <- parseInput expensesP "01/test_input.txt"
    241861950 @=? findThreeMatch expenses 


testA = TestList
    [ TestLabel "Expenses" testExpenses]

testB = TestList
    [ TestLabel "Three Expenses" testThreeExpenses]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()