import Test.HUnit
import Intcode
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    runTestTT testList
    return ()


testList = TestList
    [ TestLabel "Arithmetic Operations 1" testArithmeticOps1
    , TestLabel "Arithmetic Operations 2" testArithmeticOps2
    , TestLabel "Arithmetic Operations 3" testArithmeticOps3
    , TestLabel "Arithmetic Operations 4" testArithmeticOps4
    , TestLabel "Arithmetic Operations 5" testArithmeticOps5
    , TestLabel "Day 02 Puzzle Part A" testPuzzle02A
    , TestLabel "IO Operations 1" testIOOps1
    , TestLabel "IO Operations 2" testIOOps2
    , TestLabel "Comparison Operations 1" testCompare1
    , TestLabel "Comparison Operations 2" testCompare2
    , TestLabel "Comparison Operations 3" testCompare3
    , TestLabel "Comparison Operations 4" testCompare4
    , TestLabel "Comparison Operations 5" testCompare5
    , TestLabel "Comparison Operations 6" testCompare6
    , TestLabel "Comparison Operations 7" testCompare7
    , TestLabel "Comparison Operations 8" testCompare8
    , TestLabel "Jump Operations 1" testJump1
    , TestLabel "Jump Operations 2" testJump2
    , TestLabel "Jump Operations 3" testJump3
    , TestLabel "Jump Operations 4" testJump4
    , TestLabel "Full Comparison 1" testFullComparison1
    , TestLabel "Full Comparison 2" testFullComparison2
    , TestLabel "Full Comparison 3" testFullComparison3]


testArithmeticOps1 :: Test
testArithmeticOps1 = TestCase $ do
    (Just prog)  <- programFromString "1,9,10,3,2,3,11,0,99,30,40,50"
    (Just prog') <- programFromString "3500,9,10,70,2,3,11,0,99,30,40,50"
    let (prog'', _) = runProgram prog []
    prog' @=? prog''


testArithmeticOps2 :: Test
testArithmeticOps2 = TestCase $ do
    (Just prog)  <- programFromString "1,0,0,0,99"
    (Just prog') <- programFromString "2,0,0,0,99"
    let (prog'', _) = runProgram prog []
    prog' @=? prog''


testArithmeticOps3 :: Test
testArithmeticOps3 = TestCase $ do
    (Just prog)  <- programFromString "2,3,0,3,99"
    (Just prog') <- programFromString "2,3,0,6,99"
    let (prog'', _) = runProgram prog []
    prog' @=? prog''


testArithmeticOps4 :: Test
testArithmeticOps4 = TestCase $ do
    (Just prog)  <- programFromString "2,4,4,5,99,0"
    (Just prog') <- programFromString "2,4,4,5,99,9801"
    let (prog'', _) = runProgram prog []
    prog' @=? prog''


testArithmeticOps5 :: Test
testArithmeticOps5 = TestCase $ do
    (Just prog)  <- programFromString "1,1,1,4,99,5,6,0,99"
    (Just prog') <- programFromString "30,1,1,4,2,5,6,0,99"
    let (prog'', _) = runProgram prog []
    prog' @=? prog''


testPuzzle02A :: Test
testPuzzle02A = TestCase $ do
    (Just prog) <- parseInput programP "02/input.txt"
    let modifiedProg = prog V.// [(1, 12), (2, 2)]
    let (finalProg, _) = runProgram modifiedProg []
    6627023 @=? V.head finalProg


-- these take a single input and output it back
(Just progIO1)  = programFromString "3,0,4,0,99"
(Just progIO1') = programFromString "50,0,4,0,99"
(Just progIO2)  = programFromString "1002,4,3,4,33"
(Just progIO2') = programFromString "1002,4,3,4,99"


testIOOps1 :: Test
testIOOps1 = TestCase $ do
    -- the 50 here could be anything, the point is it spits back the one input
    let (p', ls) = runProgram progIO1 [50]
    progIO1' @=? p'
    [50] @=? ls


testIOOps2 :: Test
testIOOps2 = TestCase $ do
    -- the 50 here could be anything, the point is it spits back the one input
    let (p, ls) = runProgram progIO2 [50]
    progIO2' @=? p
    [50] @=? ls


-- these all do input `compare` 8 using various techniques
(Just progEqPos) = programFromString "3,9,8,9,10,9,4,9,99,-1,8"
(Just progLtPos) = programFromString "3,9,7,9,10,9,4,9,99,-1,8"
(Just progEqImm) = programFromString "3,3,1108,-1,8,3,4,3,99"
(Just progLtImm) = programFromString "3,3,1107,-1,8,3,4,3,99"


testCompare1 :: Test
testCompare1 = TestCase $ do
    let (_, ls) = runProgram progEqPos [7]
    [0] @=? ls

testCompare2 :: Test
testCompare2 = TestCase $ do
    let (_, ls) = runProgram progEqPos [8]
    [1] @=? ls


testCompare3 :: Test
testCompare3 = TestCase $ do
    let (_, ls) = runProgram progEqImm [7]
    [0] @=? ls


testCompare4 :: Test
testCompare4 = TestCase $ do
    let (_, ls) = runProgram progEqImm [8]
    [1] @=? ls


testCompare5 :: Test
testCompare5 = TestCase $ do
    let (_, ls) = runProgram progLtPos [7]
    [1] @=? ls


testCompare6 :: Test
testCompare6 = TestCase $ do
    let (_, ls) = runProgram progLtPos [8]
    [0] @=? ls


testCompare7 :: Test
testCompare7 = TestCase $ do
    let (_, ls) = runProgram progLtImm [7]
    [1] @=? ls


testCompare8 :: Test
testCompare8 = TestCase $ do
    let (_, ls) = runProgram progLtImm [8]
    [0] @=? ls


-- these test jumping, take one input and output 0 if output is zero and 1 else
(Just progJumpPos) = programFromString "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
(Just progJumpImm) = programFromString "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"


testJump1 :: Test
testJump1 = TestCase $ do
    let (_, ls) = runProgram progJumpPos [0]
    [0] @=? ls


testJump2 :: Test
testJump2 = TestCase $ do
    let (_, ls) = runProgram progJumpPos [1]
    [1] @=? ls


testJump3 :: Test
testJump3 = TestCase $ do
    let (_, ls) = runProgram progJumpImm [0]
    [0] @=? ls


testJump4 :: Test
testJump4 = TestCase $ do
    let (_, ls) = runProgram progJumpImm [1]
    [1] @=? ls


-- full comparison against 8 (LT -> 999, EQ -> 1000, GT -> 1001)
(Just progFullComparison) = programFromString "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"


testFullComparison1 :: Test
testFullComparison1 = TestCase $ do
    let (_, ls) = runProgram progFullComparison [7]
    [999] @=? ls


testFullComparison2 :: Test
testFullComparison2 = TestCase $ do
    let (_, ls) = runProgram progFullComparison [8]
    [1000] @=? ls


testFullComparison3 :: Test
testFullComparison3 = TestCase $ do
    let (_, ls) = runProgram progFullComparison [9]
    [1001] @=? ls





