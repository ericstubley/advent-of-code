#! /usr/bin/env python3

import unittest
import puzzle_2017_09 as puz


class Test_2017_09(unittest.TestCase):
    def test_scan_garbage(self):
        self.assertEqual(puz.scan_garbage("<>", 0), (0,len("<>")))
        self.assertEqual(puz.scan_garbage("<random characters>", 0), (17,len("<random characters>")))
        self.assertEqual(puz.scan_garbage("<<<<>", 0), (3,len("<<<<>")))
        self.assertEqual(puz.scan_garbage("<{!>}>", 0), (2,len("<{!>}>")))
        self.assertEqual(puz.scan_garbage("<!!>", 0), (0,len("<!!>")))
        self.assertEqual(puz.scan_garbage("<!!!>>", 0), (0,len("<!!!>>")))
        self.assertEqual(puz.scan_garbage("<{o\"i!a,<{i<a>", 0), (10,len("<{o\"i!a,<{i<a>")))


    def test_score_stream(self):
        self.assertEqual(puz.score_stream("{}"), 1)
        self.assertEqual(puz.score_stream("{{{}}}"), 6)
        self.assertEqual(puz.score_stream("{{},{}}"), 5)
        self.assertEqual(puz.score_stream("{{{},{},{{}}}}"), 16)
        self.assertEqual(puz.score_stream("{<a>,<a>,<a>,<a>}"), 1)
        self.assertEqual(puz.score_stream("{{<ab>},{<ab>},{<ab>},{<ab>}}"), 9)
        self.assertEqual(puz.score_stream("{{<!!>},{<!!>},{<!!>},{<!!>}}"), 9)
        self.assertEqual(puz.score_stream("{{<a!>},{<a!>},{<a!>},{<ab>}}"), 3)


    def test_count_garbage(self):
        self.assertEqual(puz.count_garbage("<>"), 0)
        self.assertEqual(puz.count_garbage("<random characters>"), 17)
        self.assertEqual(puz.count_garbage("<<<<>"), 3)
        self.assertEqual(puz.count_garbage("<{!>}>"), 2)
        self.assertEqual(puz.count_garbage("<!!>"), 0)
        self.assertEqual(puz.count_garbage("<!!!>>"), 0)
        self.assertEqual(puz.count_garbage("<{o\"i!a,<{i<a>"), 10)

if __name__ == "__main__":
    unittest.main()
