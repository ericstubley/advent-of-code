import Test.HUnit
import Puzzle_2019_05
import Intcode

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

(Just prog1) = programFromString "3,0,4,0,99"
(Just prog1') = programFromString "50,0,4,0,99"
(Just prog2) = programFromString "1002,4,3,4,33"
(Just prog2') = programFromString "1002,4,3,4,99"


(Just prog8EqPos) = programFromString "3,9,8,9,10,9,4,9,99,-1,8"
(Just prog8LtPos) = programFromString "3,9,7,9,10,9,4,9,99,-1,8"
(Just prog8EqImm) = programFromString "3,3,1108,-1,8,3,4,3,99"
(Just prog8LtImm) = programFromString "3,3,1107,-1,8,3,4,3,99"

(Just progJumpPos) = programFromString "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
(Just progJumpImm) = programFromString "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"

(Just progLarge) = programFromString "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

testIOIC :: Test
testIOIC = TestCase $ do
    putStrLn "input 50 if asked"
    prog1Final <- execute prog1
    prog2Final <- execute prog2
    prog1' @=? prog1Final
    prog2' @=? prog2Final


testCompare :: Test
testCompare = TestCase $ do
    putStrLn "expecting 8 -> 1"
    execute prog8EqPos
    putStrLn "expecting 8 -> 1"
    execute prog8EqImm
    putStrLn "expecting 7 -> 0"
    execute prog8EqPos
    putStrLn "expecting 7 -> 0"
    execute prog8EqImm
    putStrLn "expecting 7 -> 1"
    execute prog8LtPos
    putStrLn "expecting 7 -> 1"
    execute prog8LtImm
    putStrLn "expecting 8 -> 0"
    execute prog8LtPos
    putStrLn "expecting 8 -> 0"
    execute prog8LtImm
    return ()


testJump :: Test
testJump = TestCase $ do
    putStrLn "expecting 0 -> 0" 
    execute progJumpPos
    putStrLn "expecting 0 -> 0" 
    execute progJumpImm
    putStrLn "expecting 1 -> 1"
    execute progJumpPos
    putStrLn "expecting 1 -> 1"
    execute progJumpImm
    return ()


testLarge :: Test
testLarge = TestCase $ do
    putStrLn "expecting 7 -> 999"
    execute progLarge
    putStrLn "expecting 8 -> 1000"
    execute progLarge
    putStrLn "expecting 9 -> 1001"
    execute progLarge
    return ()


testA = TestList
    [ TestLabel "IO Intcode with Modes" testIOIC]

testB = TestList
    [ TestLabel "Comparisons" testCompare
    , TestLabel "Jumps" testJump
    , TestLabel "Large program" testLarge]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()