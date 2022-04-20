import Test.HUnit
import System.IO
import Puzzle_2018_20

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

r1 :: String
r2 :: String
r3 :: String
r4 :: String
r5 :: String
r1 = "^WNE$"
r2 = "^ENWWW(NEEE|SSE(EE|N))$"
r3 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
r4 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
r5 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"


testPrinting = TestCase (do
    let m1 = graphFromRegex r1
    let m2 = graphFromRegex r2
    let m3 = graphFromRegex r3
    printMap m1
    putStrLn "Does this map look okay?"
    confirm1 <- getLine
    "y" @=? confirm1
    printMap m2
    putStrLn "Does this map look okay?"
    confirm2 <- getLine
    "y" @=? confirm2
    printMap m3
    putStrLn "Does this map look okay?"
    confirm3 <- getLine
    "y" @=? confirm3
    )


testFurthest = TestCase (do
    3  @=? furthestRoom r1
    10 @=? furthestRoom r2
    18 @=? furthestRoom r3
    23 @=? furthestRoom r4
    31 @=? furthestRoom r5
    )

testA = TestList
    [ TestLabel "Graph parsing" testPrinting
    , TestLabel "Furthest room" testFurthest]

testB = TestList
    []

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()