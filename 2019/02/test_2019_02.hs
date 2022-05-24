import Test.HUnit
import Puzzle_2019_02
import Intcode
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

(Just prog1) = programFromString "1,9,10,3,2,3,11,0,99,30,40,50"
(Just prog2) = programFromString "1,0,0,0,99"
(Just prog3) = programFromString "2,3,0,3,99"
(Just prog4) = programFromString "2,4,4,5,99,0"
(Just prog5) = programFromString "1,1,1,4,99,5,6,0,99"
(Just prog1After) = programFromString "3500,9,10,70,2,3,11,0,99,30,40,50"
(Just prog2After) = programFromString "2,0,0,0,99"
(Just prog3After) = programFromString "2,3,0,6,99"
(Just prog4After) = programFromString "2,4,4,5,99,9801"
(Just prog5After) = programFromString "30,1,1,4,2,5,6,0,99"


testExecute :: Test
testExecute = TestCase (do
    let (prog1', _) = runProgram prog1 []
    let (prog2', _) = runProgram prog2 []
    let (prog3', _) = runProgram prog3 []
    let (prog4', _) = runProgram prog4 []
    let (prog5', _) = runProgram prog5 []
    prog1After @=? prog1'
    prog2After @=? prog2'
    prog3After @=? prog3'
    prog4After @=? prog4'
    prog5After @=? prog5')


testA = TestList
    [TestLabel "Program Execution" testExecute]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()