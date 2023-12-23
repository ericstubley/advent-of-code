import Test.HUnit
import Puzzle_2023_19
import Parsing (parseInput)
import qualified Data.Map.Strict as M

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testRunWorkflows = TestCase $ do
    (Just (workflows, parts)) <- parseInput inputP "19/test_input.txt"
    let flowMap = M.fromList $ zip (map _name workflows) (workflows)
    [Accept, Reject, Accept, Reject, Accept] @=? map (runWorkflows flowMap) parts

testSolveA = TestCase $ do
    (Just (workflows, parts)) <- parseInput inputP "19/test_input.txt"
    19114 @=? solveA workflows parts

testRestrictRemain = TestCase $ do
    let c = Constraint X '<' 2000
    let lower1 = Part 1 1 1 1
    let upper1 = Part 1999 4000 4000 4000
    (lower1, upper1) @=? restrict c initLower initUpper
    let lower2 = Part 2000 1 1 1
    let upper2 = Part 4000 4000 4000 4000
    (lower2, upper2) @=? remain c initLower initUpper


testSolveHelperB = TestCase $ do
    (Just (workflows, _)) <- parseInput inputP "19/test_input.txt"
    4000^4 @=? solveHelperB "lnx" workflows
    let ans = (4000-2662)*4000*4000*4000
    ans @=? solveHelperB "crn" workflows

testSolveB = TestCase $ do
    (Just (workflows, _)) <- parseInput inputP "19/test_input.txt"
    167409079868000 @=? solveB workflows

testA = TestList
    [ TestLabel "Run workflows" testRunWorkflows
    , TestLabel "Solve A" testSolveA]

testB = TestList
    [ TestLabel "RestrictRemain" testRestrictRemain
    , TestLabel "Solve debugging B" testSolveHelperB
    , TestLabel "Solve B" testSolveB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()