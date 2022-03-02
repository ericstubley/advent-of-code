#! /usr/bin/env python3

import unittest
import puzzle_2017_02 as puz


class Test_2017_02(unittest.TestCase):
    def setUp(self):
        with open("test_input_diff.txt") as f:
            data = f.readlines()
        self.rows_diff = puz.parse(data)
        with open("test_input_quot.txt") as f:
            data = f.readlines()
        self.rows_quot = puz.parse(data)

    def test_checksum(self):
        self.assertEqual(puz.checksum(self.rows_diff), 18)

    def test_quotient_checksum(self):
        self.assertEqual(puz.quotient_checksum(self.rows_quot), 9)


if __name__ == "__main__":
    unittest.main()
