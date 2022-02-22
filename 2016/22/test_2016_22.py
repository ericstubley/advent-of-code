#! /usr/bin/env python3

import unittest
import puzzle_2016_22 as puz


class Test_2016_22(unittest.TestCase):
    def setUp(self):
        with open("input.txt") as f:
            data = f.readlines()
        self.used, self.available = puz.parse(data, 30)


    def test_parse(self):
        self.assertEqual(self.used[18][24], 67)
        self.assertEqual(self.available[18][24], 25)


if __name__ == "__main__":
    unittest.main()
