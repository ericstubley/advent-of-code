import Test.HUnit
import Puzzle_2022_06

-- testGamma = TestCase (do
--     report <- parsedInput
--     let gr = binaryToInt $ gammaRate report
--     22 @=? gr)

testPacketStart = TestCase $ do
    7 @=? packetStart "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    5 @=? packetStart "bvwbjplbgvbhsrlpgdmjqwftvncz"
    6 @=? packetStart "nppdvjthqldpwncqszvftbrmjlhg"
    10 @=? packetStart "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    11 @=? packetStart "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

testMessageStart = TestCase $ do
    19 @=? messageStart "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    23 @=? messageStart "bvwbjplbgvbhsrlpgdmjqwftvncz"
    23 @=? messageStart "nppdvjthqldpwncqszvftbrmjlhg"
    29 @=? messageStart "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    26 @=? messageStart "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

testA = TestList
    [ TestLabel "Packet Start" testPacketStart]

testB = TestList
    [ TestLabel "Message Start" testMessageStart]

testList = TestList
    [ TestLabel "Part A" testA
    , TestLabel "Part B" testB]

main :: IO ()
main = do
    runTestTT testList
    return ()