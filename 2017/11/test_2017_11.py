#! /usr/bin/env python3

import unittest
import puzzle_2017_11 as puz


class Test_2017_11(unittest.TestCase):
    def test_hex_vector(self):
        self.assertEqual(puz.HexVector(1, 1).distance, 2)
        self.assertEqual(puz.HexVector(1, -1).distance, 1)
        self.assertEqual(puz.HexVector(1, -2).distance, 2)
        self.assertEqual(puz.HexVector(5, 3).distance, 8)


    def test_hex_distance(self):
        self.assertEqual(puz.hex_distance(["ne", "ne", "ne"]), 3)
        self.assertEqual(puz.hex_distance(["ne", "ne", "sw", "sw"]), 0)
        self.assertEqual(puz.hex_distance(["ne", "ne", "s", "s"]), 2)
        self.assertEqual(puz.hex_distance(["se", "sw", "se", "sw", "sw"]), 3)


if __name__ == "__main__":
    unittest.main()
