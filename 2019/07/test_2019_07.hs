import Test.HUnit
import Puzzle_2019_07
import Intcode

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

(Just prog1) = programFromString "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
(Just prog2) = programFromString "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
(Just prog3) = programFromString "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
(Just prog4) = programFromString "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
(Just prog5) = programFromString "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"


testPhaseThrust :: Test
testPhaseThrust = TestCase $ do
    let pt1 = phaseThrust prog1 [4,3,2,1,0]
    let pt2 = phaseThrust prog2 [0,1,2,3,4]
    let pt3 = phaseThrust prog3 [1,0,4,3,2]
    43210 @=? pt1
    54321 @=? pt2
    65210 @=? pt3


testMaxThrust :: Test
testMaxThrust = TestCase $ do
    let mt1 = maxThrust prog1
    let mt2 = maxThrust prog2
    let mt3 = maxThrust prog3
    43210 @=? mt1
    54321 @=? mt2
    65210 @=? mt3


testFeedbackThrust :: Test
testFeedbackThrust = TestCase $ do
    let ft1 = feedbackThrust prog4 [9,8,7,6,5]
    let ft2 = feedbackThrust prog5 [9,7,8,5,6]
    139629729 @=? ft1 
    18216 @=? ft2


testMaxFeedbackThrust :: Test
testMaxFeedbackThrust = TestCase $ do
    let mt1 = maxFeedbackThrust prog4
    let mt2 = maxFeedbackThrust prog5
    139629729 @=? mt1 
    18216 @=? mt2


testA = TestList
    [ TestLabel "Phase Thrust Test" testPhaseThrust
    , TestLabel "Max Thrust Test" testMaxThrust]

testB = TestList
    [ TestLabel "Feedback Thrust Test" testFeedbackThrust
    , TestLabel "Max Feedback Thrust Test" testMaxFeedbackThrust]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()