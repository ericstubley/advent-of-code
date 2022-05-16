import Test.HUnit
import Puzzle_2019_18
import Parsing
import Control.Monad.Reader
import qualified Data.Set as S

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)




-- testOptions :: Test
-- testOptions = TestCase $ do
--     (Just maze3) <- parseInput mazeP "18/test_input_3.txt"
--     (Just maze4) <- parseInput mazeP "18/test_input_4.txt"
--     (Just maze5) <- parseInput mazeP "18/test_input_5.txt"
--     let env3 = buildEnvironment maze3
--     let env4 = buildEnvironment maze4
--     let env5 = buildEnvironment maze5
--     let found3 = options env3 S.empty (start env3)
--     let found4 = options env4 S.empty (start env4)
--     let found5 = options env5 S.empty (start env5)
--     let found3' = map (\x -> (fst . snd $ x, fst x)) found3 
--     let found4' = map (\x -> (fst . snd $ x, fst x)) found4 
--     let found5' = map (\x -> (fst . snd $ x, fst x)) found5
--     [('a', 2), ('b', 22)] @=? found3'
--     [] @=? found4'
--     [('d', 3), ('e', 5), ('f', 7), ('a', 15)] @=? found5'

testFindKeys :: Test
testFindKeys = TestCase $ do
    (Just maze1) <- parseInput mazeP "18/test_input_1.txt"
    (Just maze2) <- parseInput mazeP "18/test_input_2.txt"
    (Just maze3) <- parseInput mazeP "18/test_input_3.txt"
    (Just maze4) <- parseInput mazeP "18/test_input_4.txt"
    (Just maze5) <- parseInput mazeP "18/test_input_5.txt"
    -- print $ buildGraph $ buildEnvironment maze1
    8   @=? findKeys maze1
    86  @=? findKeys maze2
    132 @=? findKeys maze3
    136 @=? findKeys maze4
    81  @=? findKeys maze5


testFindKeysB :: Test
testFindKeysB = TestCase $ do
    (Just maze6) <- parseInput mazeP "18/test_input_6.txt"
    (Just maze7) <- parseInput mazeP "18/test_input_7.txt"
    (Just maze8) <- parseInput mazeP "18/test_input_8.txt"
    (Just maze9) <- parseInput mazeP "18/test_input_9.txt"
    -- print $ buildGraphB $ buildEnvironmentB maze6
    8   @=? findKeysB maze6
    24  @=? findKeysB maze7
    32  @=? findKeysB maze8
    72  @=? findKeysB maze9

testA = TestList
    -- [ TestLabel "Options" testOptions
    [ TestLabel "Find all keys" testFindKeys]

testB = TestList
    [ TestLabel "Find Keys B" testFindKeysB]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()