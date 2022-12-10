import Test.HUnit
import Puzzle_2022_10
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testRegister = TestCase $ do
    (Just program) <- parseInput programP "10/test_input.txt"
    [1, 1, 1, 4, 4] @=? register program 


testSignalStrength = TestCase $ do
    (Just program) <- parseInput programP "10/test_input_2.txt"
    let correct = zipWith (*) [1..] [1, 1, 16, 16, 5, 5, 11, 11]
    correct @=? (take 8 $ signalStrengths program)


testInterestingSignals = TestCase $ do
    (Just program) <- parseInput programP "10/test_input_2.txt"
    [420, 1140, 1800, 2940, 2880, 3960] @=? (arithmeticProgression 20 40 . signalStrengths $ program) 
    13140 @=? sumInterestingSignalStrengths program 


testDraw = TestCase $ do
    (Just program) <- parseInput programP "10/test_input_2.txt"
    putStrLn "correct one"
    mapM_ putStrLn [ "##..##..##..##..##..##..##..##..##..##.."
                   , "###...###...###...###...###...###...###."
                   , "####....####....####....####....####...."
                   , "#####.....#####.....#####.....#####....."
                   , "######......######......######......####"
                   , "#######.......#######.......#######....."]
    putStrLn "program one"
    mapM_ putStrLn . screen $ program 
    putStrLn "are they the same? y/n"
    result <- getChar
    'y' @=? result



testA = TestList
    [ testRegister
    , testSignalStrength
    , testInterestingSignals]

testB = TestList
    [ testDraw]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()