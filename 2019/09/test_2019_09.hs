import Test.HUnit
import Puzzle_2019_09
import Intcode

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testQuine :: Test
testQuine = TestCase $ do
    let (Just p) = programFromString "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    let (_, out) = runProgram p []
    p @=? out


testLarge1 :: Test
testLarge1 = TestCase $ do
    let (Just p) = programFromString "1102,34915192,34915192,7,4,7,99,0"
    let (_, out) = runProgram p []
    (34915192^2) @=? head out


testLarge2 :: Test
testLarge2 = TestCase $ do
    let (Just p) = programFromString "104,1125899906842624,99"
    let (_, out) = runProgram p []
    1125899906842624 @=? head out

testA = TestList
    [ TestLabel "Quine with Rel Mode" testQuine
    , TestLabel "Large Numbers 1" testLarge1
    , TestLabel "Large Numbers 2" testLarge2]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()