import Test.HUnit
import Puzzle_2022_01
import Parsing (parseInput)


-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testParse = TestCase $ do
    (Just elves) <- parseInput elvesP "01/test_input.txt"
    elves @=? [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]


testMax = TestCase $ do
    (Just elves) <- parseInput elvesP "01/test_input.txt"
    let m = findMax elves
    24000 @=? m


testTopThree = TestCase $ do
    (Just elves) <- parseInput elvesP "01/test_input.txt"
    let t = topThree $ map sum elves
    t @=? (24000, 11000, 10000)


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Max" testMax]

testB = TestList
    [ TestLabel "Top Three" testTopThree]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()