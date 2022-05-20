import Test.HUnit
import Puzzle_2019_24
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse :: Test
testParse = TestCase $ do
    (Just bugs0) <- parseInput bugsP "24/test_input_0.txt"
    let grid = [ "....#"
               , "#..#."
               , "#..##"
               , "..#.."
               , "#...."] 
    (2^4 + 2^5 + 2^8 + 2^10 + 2^13 + 2^14 + 2^17 + 2^20) @=? readBugs grid
    (2^4 + 2^5 + 2^8 + 2^10 + 2^13 + 2^14 + 2^17 + 2^20) @=? bugs0
    grid @=? showBugs bugs0


testNeighbours :: Test
testNeighbours = TestCase $ do
    (Just bugs0) <- parseInput bugsP "24/test_input_0.txt"
    printBugs bugs0
    putStrLn ""
    printBugs (left  bugs0)
    putStrLn ""
    printBugs (right bugs0)
    putStrLn ""
    printBugs (above bugs0)
    putStrLn ""
    printBugs (below bugs0)
    putStrLn ""
    printBugs (oneNeighbour bugs0)
    putStrLn ""
    printBugs (twoNeighbours bugs0)
    putStrLn ""
    printBugs (step  bugs0)



testRecursive :: Test
testRecursive = TestCase $ do
    (Just bugs) <- parseInput mapP "24/test_input_0.txt"
    let bugs' = evolve 10 bugs
    99 @=? countBugs bugs'



testStep :: Test
testStep = TestCase $ do
    (Just bugs0) <- parseInput bugsP "24/test_input_0.txt"
    (Just bugs1) <- parseInput bugsP "24/test_input_1.txt"
    (Just bugs2) <- parseInput bugsP "24/test_input_2.txt"
    (Just bugs3) <- parseInput bugsP "24/test_input_3.txt"
    (Just bugs4) <- parseInput bugsP "24/test_input_4.txt"

    bugs1 @=? step bugs0
    bugs2 @=? step bugs1
    bugs3 @=? step bugs2
    bugs4 @=? step bugs3


testRepeat :: Test
testRepeat = TestCase $ do
    (Just bugs0) <- parseInput bugsP "24/test_input_0.txt"
    (Just bugs5) <- parseInput bugsP "24/test_input_5.txt"

    bugs5 @=? firstRepeat bugs0


testA = TestList
    [ TestLabel "Parsing" testParse
    -- , TestLabel "Visual Operations" testNeighbours
    , TestLabel "Time Step" testStep
    , TestLabel "First Repeat" testRepeat]

testB = TestList
    [ TestLabel "Recursive" testRecursive]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()