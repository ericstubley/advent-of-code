#! /usr/bin/env python3

import unittest
import puzzle_2016_20 as puz


class Test_2016_20(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()
        self.intervals = puz.parse(data)

    def test_parse(self):
        self.assertListEqual(self.intervals, [(5, 8), (0, 2), (4, 7)])

    def test_first_available(self):
        self.assertEqual(puz.first_available(self.intervals), 3)

    def test_num_allowed(self):
        self.assertEqual(puz.num_allowed(self.intervals, 9), 2)


if __name__ == "__main__":
    unittest.main()
