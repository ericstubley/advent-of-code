import Test.HUnit
import Puzzle_2019_10
import Grid
import Parsing

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


testVisibleAsteroids :: Test
testVisibleAsteroids = TestCase $ do
    (Just asteroidField1) <- parseInput asteroidFieldP "10/test_input_1.txt"
    (Just asteroidField2) <- parseInput asteroidFieldP "10/test_input_2.txt"
    (Just asteroidField3) <- parseInput asteroidFieldP "10/test_input_3.txt"
    (Just asteroidField4) <- parseInput asteroidFieldP "10/test_input_4.txt"
    (Just asteroidField5) <- parseInput asteroidFieldP "10/test_input_5.txt"
    8   @=? numberVisible asteroidField1 (4:.3)
    33  @=? numberVisible asteroidField2 (8:.5)
    35  @=? numberVisible asteroidField3 (2:.1)
    41  @=? numberVisible asteroidField4 (3:.6)
    210 @=? numberVisible asteroidField5 (13:.11)


testBestAsteroid :: Test
testBestAsteroid = TestCase $ do
    (Just asteroidField1) <- parseInput asteroidFieldP "10/test_input_1.txt"
    (Just asteroidField2) <- parseInput asteroidFieldP "10/test_input_2.txt"
    (Just asteroidField3) <- parseInput asteroidFieldP "10/test_input_3.txt"
    (Just asteroidField4) <- parseInput asteroidFieldP "10/test_input_4.txt"
    (Just asteroidField5) <- parseInput asteroidFieldP "10/test_input_5.txt"
    (4:.3)   @=? bestAsteroid asteroidField1
    (8:.5)   @=? bestAsteroid asteroidField2
    (2:.1)   @=? bestAsteroid asteroidField3
    (3:.6)   @=? bestAsteroid asteroidField4
    (13:.11) @=? bestAsteroid asteroidField5


testDestructionOrder :: Test
testDestructionOrder = TestCase $ do
    (Just asteroidField6) <- parseInput asteroidFieldP "10/test_input_6.txt"
    let order = destructionOrder asteroidField6 (3:.8)
    (1:.8) @=? order !! 0
    (0:.9) @=? order !! 1
    (1:.9) @=? order !! 2
    (0:.10) @=? order !! 3

    -- print order

-- .#....###24...#..
-- ##...##.13#67..9#
-- ##...#...5.8####.
-- ..#.....X...###..
-- ..#.#.....#....##


testDestructionOrder' :: Test
testDestructionOrder' = TestCase $ do
    (Just asteroidField5) <- parseInput asteroidFieldP "10/test_input_5.txt"
    let dO = destructionOrder asteroidField5 (13:.11)
    [(12:.11), (1:.12), (2:.12)] @=? take 3 dO
    (8:.12) @=? dO !! 9
    (0:.16) @=? dO !! 19
    (9:.16) @=? dO !! 49
    (16:.10) @=? dO !! 99 
    (6:.9) @=? dO !! 198
    (2:.8) @=? dO !! 199
    (9:.10) @=? dO !! 200
    (1:.11) @=? dO !! 298


testA = TestList
    [ TestLabel "Visible Asteroids" testVisibleAsteroids
    , TestLabel "Best Asteroid" testBestAsteroid]

testB = TestList
    [ TestLabel "Destruction Order" testDestructionOrder
    , TestLabel "Destruction Order Large" testDestructionOrder']

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()