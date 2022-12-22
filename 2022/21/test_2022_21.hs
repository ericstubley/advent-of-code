import Test.HUnit
import Puzzle_2022_21
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testEvaluate = TestCase $ do
    (Just monkeys) <- parseInput monkeysP "21/test_input.txt"
    152 @=? evaluate monkeys "root" 


testTrack = TestCase $ do
    (Just monkeys) <- parseInput monkeysP "21/test_input.txt"
    ["root", "pppw", "cczh", "lgvd", "ptdq", "humn"] @=? track monkeys "root"


testRiddle = TestCase $ do
    (Just monkeys) <- parseInput monkeysP "21/test_input.txt"
    301 @=? riddle monkeys


testA = TestList
    [ TestLabel "Evaluation" testEvaluate]

testB = TestList
    [ TestLabel "Track" testTrack
    , TestLabel "Riddle" testRiddle]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()