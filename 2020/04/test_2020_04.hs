import Test.HUnit
import Puzzle_2020_04
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testValidity :: Test
testValidity = TestCase $ do
    (Just passports) <- parseInput passportsP "04/test_input.txt"
    4 @=? length passports
    True  @=? isValid (passports !! 0)
    False @=? isValid (passports !! 1)
    True  @=? isValid (passports !! 2)
    False @=? isValid (passports !! 3) 


testStrict :: Test
testStrict = TestCase $ do
    (Just invalids) <- parseInput passportsP "04/test_invalid.txt"
    (Just valids) <- parseInput passportsP "04/test_valid.txt"
    4 @=? length invalids
    4 @=? length valids
    [False, False, False, False] @=? map strictValidator invalids
    [True, True, True, True] @=? map strictValidator valids


testA = TestList
    [ TestLabel "Validity" testValidity]

testB = TestList
    [ TestLabel "Strict Validation" testStrict]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()