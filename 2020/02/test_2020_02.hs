import Test.HUnit
import Puzzle_2020_02
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testValidity :: Test
testValidity = TestCase $ do
    (Just database) <- parseInput databaseP "02/test_input.txt"
    let (r0, p0) = database !! 0 
    let (r1, p1) = database !! 1 
    let (r2, p2) = database !! 2 
    True  @=? isValid r0 p0
    False @=? isValid r1 p1
    True  @=? isValid r2 p2


testPositional :: Test
testPositional = TestCase $ do
    (Just database) <- parseInput databaseP "02/test_input.txt"
    let (r0, p0) = database !! 0 
    let (r1, p1) = database !! 1 
    let (r2, p2) = database !! 2 
    True  @=? isPositional r0 p0
    False @=? isPositional r1 p1
    False @=? isPositional r2 p2


testA = TestList
    [ TestLabel "Validity" testValidity]

testB = TestList
    [ TestLabel "Positional Validity" testPositional]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()