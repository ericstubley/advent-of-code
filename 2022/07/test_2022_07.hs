import Test.HUnit
import Puzzle_2022_07
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase $ do
    (Just simpleRoot) <- parseInput commandP "07/test_input_simple.txt"
    let simpleCorrect = Directory "top" [File "a" 1000]
    simpleRoot @=? simpleCorrect
    (Just root) <- parseInput commandP "07/test_input.txt"
    let correct = Directory "/" [ File "b.txt" 14848514
                                , File "c.dat" 8504156 
                                , Directory "a" [ File "f" 29116
                                                , File "g" 2557
                                                , File "h.lst" 62596
                                                , Directory "e" [File "i" 584]]
                                , Directory "d" [ File "j" 4060174
                                                , File "d.log" 8033020
                                                , File "d.ext" 5626152
                                                , File "k" 7214296]]
    root @=? correct


testSize = TestCase $ do
    (Just root) <- parseInput commandP "07/test_input.txt"
    let size = totalSize root
    size @=? 48381165

testSizes = TestCase $ do
    (Just root) <- parseInput commandP "07/test_input.txt"
    let correct = [48381165, 94853, 584, 24933642]
    correct @=? sizes root

testTotalThresholdSize = TestCase $ do
    (Just root) <- parseInput commandP "07/test_input.txt"
    let tss = totalThresholdSize 100000 root
    tss @=? 95437


testSelection = TestCase $ do
    (Just root) <- parseInput commandP "07/test_input.txt"
    24933642 @=? select 70000000 30000000 root


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Size" testSize
    , TestLabel "Sizes" testSizes
    , TestLabel "Threshold Size Finding" testTotalThresholdSize]

testB = TestList
    [ TestLabel "Directory Selection" testSelection]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()