import Test.HUnit
import System.IO
import Puzzle_2019_01

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testFuel :: Test
testFuel = TestCase (do
    2 @=? fuel 12
    2 @=? fuel 14
    654 @=? fuel 1969
    33583 @=? fuel 100756)


testFuel' :: Test
testFuel' = TestCase (do
    2 @=? fuel' 14
    966 @=? fuel' 1969
    50346 @=? fuel' 100756)


testA = TestList
    [ TestLabel "Fuel Computation" testFuel]

testB = TestList
    [ TestLabel "Fuel' Computation" testFuel']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()