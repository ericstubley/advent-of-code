import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as V
import Test.HUnit
import System.IO
import Puzzle_2018_19

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO (Int, Program)
parsedInput = parseInput "test_input.txt"


testParse = TestCase (do
    (ip, program) <- parsedInput
    7 @=? S.length program)


testProgram = TestCase (do
    (ip, program) <- parsedInput
    let p1 = S.take 1 program
    let p2 = S.take 2 program
    let p3 = S.take 3 program
    let p4 = S.take 4 program
    let p5 = S.take 5 program
    let p6 = S.take 6 program
    let p7 = S.take 7 program
    V.fromList [1, 5, 0, 0, 0, 0] @=? runProgram p1 ip (V.replicate 6 0)
    V.fromList [2, 5, 6, 0, 0, 0] @=? runProgram p2 ip (V.replicate 6 0)
    V.fromList [4, 5, 6, 0, 0, 0] @=? runProgram p3 ip (V.replicate 6 0)
    V.fromList [4, 5, 6, 0, 0, 0] @=? runProgram p4 ip (V.replicate 6 0)
    V.fromList [6, 5, 6, 0, 0, 0] @=? runProgram p5 ip (V.replicate 6 0)
    V.fromList [6, 5, 6, 0, 0, 0] @=? runProgram p6 ip (V.replicate 6 0)
    V.fromList [7, 5, 6, 0, 0, 9] @=? runProgram p7 ip (V.replicate 6 0))


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Program" testProgram]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()