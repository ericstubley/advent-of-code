import Data.Massiv.Array (Ix2(..))
import Test.HUnit
import System.IO
import Puzzle_2018_13

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

parsedInput :: IO (Array2 Char, [Cart])
parsedInput = parseInput "test_input.txt"


testParse = TestCase (do
    (track, carts) <- parsedInput
    let cartA = Cart (3 :. 9) D Counter
    let cartB = Cart (0 :. 2) R Counter
    [cartA, cartB] @=? carts)


testTick = TestCase (do
    (track, carts) <- parsedInput
    let cartA = Cart (0 :. 3) R Counter
    let cartB = Cart (4 :. 9) R Straight
    [cartA, cartB] @=? (snd $ tick track carts))


testCollision = TestCase (do
    (track, carts) <- parsedInput
    (3 :. 7) @=? firstCollision track carts)


testLastCart = TestCase (do
    (track, carts) <- parseInput "test_input_2.txt"
    let (collided, ticked) = tick track carts
    (4 :. 6) @=? (lastCartStanding track carts))


testA = TestList
    [ TestLabel "Parsing" testParse
    , TestLabel "Ticking" testTick
    , TestLabel "Collision" testCollision]

testB = TestList
    [ TestLabel "Last Cart" testLastCart]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()