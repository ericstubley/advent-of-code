import Test.HUnit
import Puzzle_2019_22
import Parsing
import Data.Monoid (Product)

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testParse :: Test
testParse = TestCase $ do
    (Just shuffle1) <- parseInput shuffleP "22/test_input_1.txt"
    (Just shuffle2) <- parseInput shuffleP "22/test_input_2.txt"
    (Just shuffle3) <- parseInput shuffleP "22/test_input_3.txt"
    (Just shuffle4) <- parseInput shuffleP "22/test_input_4.txt"
    3 @=? length shuffle1
    3 @=? length shuffle2
    3 @=? length shuffle3
    10 @=? length shuffle4


testShuffle :: Test
testShuffle = TestCase $ do
    (Just shuffle1) <- parseInput shuffleP "22/test_input_1.txt"
    (Just shuffle2) <- parseInput shuffleP "22/test_input_2.txt"
    (Just shuffle3) <- parseInput shuffleP "22/test_input_3.txt"
    (Just shuffle4) <- parseInput shuffleP "22/test_input_4.txt"
    -- result 1 
    -- 0 1 2 3 4 5 6 7 8 9
    -- 0 3 6 9 2 5 8 1 4 7
    [0, 7, 4, 1, 8, 5, 2, 9, 6, 3] @=? (map (position shuffle1 10) [0..9])
    -- result 2
    -- 0 1 2 3 4 5 6 7 8 9
    -- 3 0 7 4 1 8 5 2 9 6
    [1, 4, 7, 0, 3, 6, 9, 2, 5, 8] @=? (map (position shuffle2 10) [0..9])
    -- result 3
    -- 0 1 2 3 4 5 6 7 8 9
    -- 6 3 0 7 4 1 8 5 2 9
    [2, 5, 8, 1, 4, 7, 0, 3, 6, 9] @=? (map (position shuffle3 10) [0..9])
    -- result 4
    -- 0 1 2 3 4 5 6 7 8 9
    -- 9 2 5 8 1 4 7 0 3 6
    [7, 4, 1, 8, 5, 2, 9, 6, 3, 0] @=? (map (position shuffle4 10) [0..9])


-- remember the set modulus = 10 near the top of the puzzle file
-- otherwise this test will be bonkers
testTechnique :: Test
testTechnique = TestCase $ do
    (Just t1) <- parseInput techniqueP "22/test_input_1.txt"
    (Just t2) <- parseInput techniqueP "22/test_input_2.txt"
    (Just t3) <- parseInput techniqueP "22/test_input_3.txt"
    (Just t4) <- parseInput techniqueP "22/test_input_4.txt"
    [0, 7, 4, 1, 8, 5, 2, 9, 6, 3] @=? (map (applyTechnique t1) [0..9])
    [1, 4, 7, 0, 3, 6, 9, 2, 5, 8] @=? (map (applyTechnique t2) [0..9])
    [2, 5, 8, 1, 4, 7, 0, 3, 6, 9] @=? (map (applyTechnique t3) [0..9])
    [7, 4, 1, 8, 5, 2, 9, 6, 3, 0] @=? (map (applyTechnique t4) [0..9])


testExponentiate :: Test
testExponentiate = TestCase $ do
    1024 @=? exponentiate (2 :: Product Int) (10 :: Int)
    32768 @=? exponentiate (2 :: Product Int) (15 :: Int)
    9765625 @=? exponentiate (5 :: Product Int) (10 :: Int)



testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Shuffling" testShuffle]

testB = TestList
    [ TestLabel "Technique" testTechnique
    , TestLabel "Exponentiate" testExponentiate]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()