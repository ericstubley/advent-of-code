import Test.HUnit
import Puzzle_2019_08

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFullestLayer :: Test
testFullestLayer = TestCase $ do
    let image = "123456789012"
    let fl = fullestLayer (3*2) image
    "123456" @=? fl


testDecode :: Test
testDecode = TestCase $ do
    "0110" @=? decode (2*2) "0222112222120000"

testA = TestList
    [ TestLabel "Fullest Layer" testFullestLayer]

testB = TestList
    [ TestLabel "Decode" testDecode]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()