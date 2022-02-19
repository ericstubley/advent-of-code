#! /usr/bin/env python3

import unittest
import puzzle_2016_19 as puz


class Test_2016_19(unittest.TestCase):
    def test_last_one_standing(self):
        self.assertEqual(puz.last_one_standing_next(5), 3)
        self.assertEqual(puz.last_one_standing_next(4), 1)
        self.assertEqual(puz.last_one_standing_next(8), 1)
        self.assertEqual(puz.last_one_standing_next(16), 1)

    def test_last_one_standing_across(self):
        self.assertEqual(puz.last_one_standing_across(5), 2)
        self.assertEqual(puz.last_one_standing_across(6), 3)


if __name__ == "__main__":
    unittest.main()
