import Test.HUnit
import Puzzle_2020_08
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testLoop :: Test
testLoop = TestCase $ do
    (Just prog) <- parseInput assemblyP "08/test_input.txt"
    5 @=? loopCheck prog


testTerminate :: Test
testTerminate = TestCase $ do
    (Just prog) <- parseInput assemblyP "08/test_input.txt"
    -- mapM_ print (terminateCheck prog)
    8 @=? terminateCheck prog 


testA = TestList
    [ TestLabel "Loop Check" testLoop]

testB = TestList
    [ TestLabel "Terminate Check" testTerminate]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()