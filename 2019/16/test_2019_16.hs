import Test.HUnit
import Puzzle_2019_16

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testDigits :: Test
testDigits = TestCase $ do
    [1,2,3] @=? toDigits 123
    123 @=? fromDigits [1,2,3]


testFFT :: Test
testFFT = TestCase $ do
    let start = toDigits 12345678
    [12345678, 48226158, 34040438, 03415518, 01029498]
        @=? ((map fromDigits) . (take 5) . (iterate fft) $ start)


testIterate :: Test
testIterate = TestCase $ do
    let s1 = toDigits 80871224585914 ++ toDigits 546619083218645595
    let s2 = toDigits 19617804207202 ++ toDigits 209144916044189917
    let s3 = toDigits 69317163492948 ++ toDigits 606335995924319873
    let e1 = iterativeFFT 100 s1 
    let e2 = iterativeFFT 100 s2 
    let e3 = iterativeFFT 100 s3 
    24176176 @=? (fromDigits $ take 8 e1) 
    73745418 @=? (fromDigits $ take 8 e2) 
    52432133 @=? (fromDigits $ take 8 e3) 


testOffset :: Test
testOffset = TestCase $ do
    let s1 = [0] ++ toDigits 0303673257721294 ++ toDigits 4063491565474664
    303673 @=? offset s1



testDecode :: Test
testDecode = TestCase $ do
    let s1 = [0] ++ toDigits 0303673257721294 ++ toDigits 4063491565474664
    let s2 = [0] ++ toDigits 0293510969994080 ++ toDigits 7407585447034323
    let s3 = [0] ++ toDigits 0308177088492195 ++ toDigits 9731165446850517
    84462026 @=? decode s1
    78725270 @=? decode s2
    53553731 @=? decode s3

testA = TestList
    [ TestLabel "Digits" testDigits
    , TestLabel "FFT" testFFT
    , TestLabel "Large iterates" testIterate]

testB = TestList
    [ TestLabel "Offset" testOffset
    , TestLabel "Decode" testDecode]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()