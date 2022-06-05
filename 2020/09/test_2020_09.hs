import Test.HUnit
import Puzzle_2020_09
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFindError :: Test
testFindError = TestCase $ do
    (Just xmas) <- parseInput xmasP "09/test_input.txt"
    127 @=? findError 5 xmas


testEncryptionWeakness :: Test
testEncryptionWeakness = TestCase $ do
    (Just xmas) <- parseInput xmasP "09/test_input.txt"
    62 @=? encryptionWeakness 5 xmas


testA = TestList
    [ TestLabel "Find Error" testFindError]

testB = TestList
    [ TestLabel "Encryption Weakness" testEncryptionWeakness]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()