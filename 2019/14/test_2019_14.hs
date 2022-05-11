import Test.HUnit
import Puzzle_2019_14
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse :: Test
testParse = TestCase $ do
    (Just reactions1) <- parseInput reactionsP "14/test_input_1.txt"
    6 @=? length reactions1


testOrder :: Test
testOrder = TestCase $ do
    (Just reactions1) <- parseInput reactionsP "14/test_input_1.txt"
    let order = topologicalSort . dependencyGraph $ reactions1
    ["FUEL", "E", "D", "C", "A", "B"] @=? order 


testOreReq :: Test
testOreReq = TestCase $ do
    (Just reactions1) <- parseInput reactionsP "14/test_input_1.txt"
    (Just reactions2) <- parseInput reactionsP "14/test_input_2.txt"
    (Just reactions3) <- parseInput reactionsP "14/test_input_3.txt"
    (Just reactions4) <- parseInput reactionsP "14/test_input_4.txt"
    (Just reactions5) <- parseInput reactionsP "14/test_input_5.txt"
    31 @=? oreReq reactions1
    165 @=? oreReq reactions2
    13312 @=? oreReq reactions3
    180697 @=? oreReq reactions4
    2210736 @=? oreReq reactions5 


testMaxFuel :: Test
testMaxFuel = TestCase $ do
    (Just reactions3) <- parseInput reactionsP "14/test_input_3.txt"
    (Just reactions4) <- parseInput reactionsP "14/test_input_4.txt"
    (Just reactions5) <- parseInput reactionsP "14/test_input_5.txt"
    82892753 @=? maxFuel reactions3 1000000000000 
    5586022  @=? maxFuel reactions4 1000000000000
    460664   @=? maxFuel reactions5 1000000000000


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Order" testOrder
    , TestLabel "Ore Requirement" testOreReq]

testB = TestList
    [ TestLabel "Max Fuel" testMaxFuel]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()