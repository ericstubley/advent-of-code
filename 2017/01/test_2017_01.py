#! /usr/bin/env python3

import unittest
import puzzle_2017_01 as puz


class Test_2017_01(unittest.TestCase):
    def test_next_match_sum(self):
        self.assertEqual(puz.next_match_sum([1, 1, 2, 2]), 3)
        self.assertEqual(puz.next_match_sum([1, 1, 1, 1]), 4)
        self.assertEqual(puz.next_match_sum([1, 2, 3, 4]), 0)
        self.assertEqual(puz.next_match_sum([9, 1, 2, 1, 2, 1, 9]), 9)

    def test_half_match_sum(self):
        self.assertEqual(puz.half_match_sum([1, 2, 1, 2]), 6)
        self.assertEqual(puz.half_match_sum([1, 2, 2, 1]), 0)
        self.assertEqual(puz.half_match_sum([1, 2, 3, 4, 2, 5]), 4)
        self.assertEqual(puz.half_match_sum([1, 2, 3, 1, 2, 3]), 12)
        self.assertEqual(puz.half_match_sum([1, 2, 1, 3, 1, 4, 1, 5]), 4)


if __name__ == "__main__":
    unittest.main()
