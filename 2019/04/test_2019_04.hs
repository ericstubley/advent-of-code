import Test.HUnit
import Puzzle_2019_04

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testDigits :: Test
testDigits = TestCase $ do
    [1, 2, 3, 4, 5, 6] @=? digits 123456

testValid :: Test
testValid = TestCase $ do
    True @=? validity 111111
    False @=? validity 223450
    False @=? validity 123789


testValid' :: Test
testValid' = TestCase $ do
    True @=? validity' 112233
    False @=? validity' 123444
    True @=? validity' 111122


testA = TestList
    [ TestLabel "Digits" testDigits
    , TestLabel "Validity" testValid]

testB = TestList
    [ TestLabel "Validity'" testValid']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()