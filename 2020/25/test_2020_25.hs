import Test.HUnit
import Puzzle_2020_25

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)
testDiscreteLog :: Test
testDiscreteLog = TestCase $ do
    8 @=? discreteLog 5764801
    11 @=? discreteLog 17807724 


testEncryptionKey :: Test
testEncryptionKey = TestCase $ do
   14897079 @=? encryptionKey 5764801 17807724



testA = TestList
    [ TestLabel "Discrete Log" testDiscreteLog
    , TestLabel "Encryption Key" testEncryptionKey]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()