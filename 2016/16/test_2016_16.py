#! /usr/bin/env python3

import unittest
import puzzle_2016_16 as puz


class Test_2016_16(unittest.TestCase):
    def setUp(self):
        self.initial = "10000"

    def test_checksum(self):
        self.assertEqual(puz.checksum("110010110100"), "100")
        self.assertEqual(puz.checksum("10000011110010000111"), "01100")

    def test_fill_to_length(self):
        self.assertEqual(puz.fill_to_length(20, "10000"), "10000011110010000111")


if __name__ == "__main__":
    unittest.main()
