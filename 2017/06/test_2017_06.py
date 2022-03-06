#! /usr/bin/env python3

import unittest
import puzzle_2017_06 as puz


class Test_2017_06(unittest.TestCase):
    def setUp(self):
        self.banks = list(map(int, "0 2 7 0".split()))


    def test_redistribute_blocks(self):
        self.assertEqual(puz.redistribute_blocks([__ for __ in self.banks]), [2, 4, 1, 2])


    def test_time_to_repeated_state(self):
        self.assertEqual(puz.time_to_repeated_state(self.banks), (5, 1))


if __name__ == "__main__":
    unittest.main()
