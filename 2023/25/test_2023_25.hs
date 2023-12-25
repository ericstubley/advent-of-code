import Test.HUnit
import Puzzle_2023_25
import Parsing (parseInput)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testComponentSize = TestCase $ do
    (Just wires) <- parseInput wiresP "25/test_input.txt"
    -- 54 @=? componentSizes wires
    let result = defaultVis . buildGraph $ es
    print result


testA = TestList
    [ TestLabel "Component Sizes" testComponentSize]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()