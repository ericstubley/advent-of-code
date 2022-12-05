import Test.HUnit
import Puzzle_2022_05
import Parsing (parseInput)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse = TestCase $ do
    (Just (crates, _)) <- parseInput craneP "05/test_input.txt" 
    let correct = V.fromList ["NZ", "DCM", "P"]
    crates @=? correct


testRearrange = TestCase $ do
    (Just (crates, instructions)) <- parseInput craneP "05/test_input.txt" 
    let rearranged = V.fromList ["C", "M", "ZNDP"] 
    rearranged @=? (rearrange crates instructions)



testRearrange9001 = TestCase $ do
    (Just (crates, instructions)) <- parseInput craneP "05/test_input.txt" 
    let rearranged = V.fromList ["M", "C", "DNZP"] 
    rearranged @=? (rearrange9001 crates instructions)



testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Rearrangement" testRearrange]

testB = TestList
    [ TestLabel "Rearrangement 9001" testRearrange9001]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()