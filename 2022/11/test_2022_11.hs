import Test.HUnit
import Puzzle_2022_11
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testKeepAwayRound = TestCase $ do
    (Just (monkeys, items)) <- parseInput monkeysP "11/test_input.txt"
    let items' = keepAwayRound monkeys items
    mapM_ (putStrLn . show) items'


testManyRounds = TestCase $ do
    (Just (monkeys, items)) <- parseInput monkeysP "11/test_input.txt"
    let items' = (iterate (keepAwayRound monkeys) items) !! 20
    mapM_ (putStrLn . show) items'


testCounts = TestCase $ do
    (Just (monkeys, items)) <- parseInput monkeysP "11/test_input.txt"
    [101, 95, 7, 105] @=? totalHeld 20 monkeys items 
    

testLongCounts = TestCase $ do
    print "Make sure you remembered to change the modulus in inspect'"
    (Just (monkeys, items)) <- parseInput monkeysP "11/test_input.txt"
    [52166, 47830, 1938, 52013] @=? longRun 10000 monkeys items

testA = TestList
    [ testKeepAwayRound
    , testManyRounds
    , testCounts]

testB = TestList
    [ testLongCounts]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()