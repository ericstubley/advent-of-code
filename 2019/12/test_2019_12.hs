import Test.HUnit
import Puzzle_2019_12
import Parsing
import Control.Monad.State
import Linear.V3 (V3(..))

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)


-- for test_input_1
-- after 1 step
-- pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
-- pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
-- pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
-- pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
-- 
-- after 2 steps
-- pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
-- pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
-- pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
-- pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>

testTimeStep :: Test
testTimeStep = TestCase $ do
    (Just moons) <- parseInput moonsP "12/test_input_1.txt"
    let moons' = execState step moons
    let moons'' = execState step moons'
    Moon (V3 2 (-1) 1) (V3 3 (-1) (-1)) @=? head moons'
    Moon (V3 5 (-3) (-1)) (V3 3 (-2) (-2)) @=? head moons''


testEnergy :: Test
testEnergy = TestCase $ do
    (Just moons1) <- parseInput moonsP "12/test_input_1.txt"
    (Just moons2) <- parseInput moonsP "12/test_input_2.txt"
    let ms1' = execState (repeatM 10 step) moons1
    let ms2' = execState (repeatM 100 step) moons2
    let e1 = sum $ map energy ms1'
    let e2 = sum $ map energy ms2'
    179 @=? e1
    1940 @=? e2


testTimeToRepeat :: Test
testTimeToRepeat = TestCase $ do
    (Just moons1) <- parseInput moonsP "12/test_input_1.txt"
    (Just moons2) <- parseInput moonsP "12/test_input_2.txt"
    2772 @=? timeToRepeat moons1
    4686774924 @=? timeToRepeat moons2


testA = TestList
    [ TestLabel "Time Evolution" testTimeStep
    , TestLabel "Energy after n Steps" testEnergy]

testB = TestList
    [ TestLabel "Time to Repeat" testTimeToRepeat]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()