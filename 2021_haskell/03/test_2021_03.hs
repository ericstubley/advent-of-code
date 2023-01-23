import Test.HUnit
import Puzzle_2021_03
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testGamma = TestCase $ do
    (Just report) <- parseInput reportP "03/test_input.txt"
    22 @=? (interpret $ gammaRate report)


testEpsilon = TestCase $ do
    (Just report) <- parseInput reportP "03/test_input.txt"
    9 @=? (interpret $ epsilonRate report)


testOxy = TestCase $ do
    (Just report) <- parseInput reportP "03/test_input.txt"
    23 @=? (interpret $ oxyRating report)


testCo2 = TestCase $ do
    (Just report) <- parseInput reportP "03/test_input.txt"
    10 @=? (interpret $ co2Rating report)


testA = TestList
    [ TestLabel "Gamma Rate" testGamma
    , TestLabel "Epsilon Rate" testEpsilon]

testB = TestList
    [ TestLabel "Oxygen Rating" testOxy
    , TestLabel "Co2 Rating" testCo2]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()