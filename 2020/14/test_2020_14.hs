import Test.HUnit
import Puzzle_2020_14
import Parsing (parseInput)
import qualified Data.Map.Strict as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testBoot :: Test
testBoot = TestCase $ do
    (Just operations) <- parseInput operationsP "14/test_input.txt"
    let memory = runProgram operations
    M.fromList [(7, 101), (8, 64)] @=? memory


testV2 :: Test
testV2 = TestCase $ do
    (Just operations) <- parseInput operationsV2P "14/test_input_2.txt"
    let memory = runProgramV2 operations
    print memory
    208 @=? valueSum memory


testA = TestList
    [ TestLabel "Boot Sequence" testBoot]

testB = TestList
    [ TestLabel "V2 Operations" testV2]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()