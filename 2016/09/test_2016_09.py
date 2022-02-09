#! /usr/bin/env python3

import unittest
import puzzle_2016_09 as puz


class Test_2016_09(unittest.TestCase):
    def test_decompressed_length_v1(self):
        self.assertEqual(puz.decompressed_length("ADVENT", False), 6)
        self.assertEqual(puz.decompressed_length("A(1x5)BC", False), 7)
        self.assertEqual(puz.decompressed_length("(3x3)XYZ", False), 9)
        self.assertEqual(puz.decompressed_length("A(2x2)BCD(2x2)EFG", False), 11)
        self.assertEqual(puz.decompressed_length("(6x1)(1x3)A", False), 6)
        self.assertEqual(puz.decompressed_length("X(8x2)(3x3)ABCY", False), 18)

    def test_decompressed_length_v2(self):
        self.assertEqual(puz.decompressed_length("(3x3)XYZ", True), 9)
        self.assertEqual(puz.decompressed_length("X(8x2)(3x3)ABCY", True), len("XABCABCABCABCABCABCY"))
        self.assertEqual(puz.decompressed_length("(27x12)(20x12)(13x14)(7x10)(1x12)A", True), 241920)
        self.assertEqual(puz.decompressed_length("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", True), 445)


if __name__ == "__main__":
    unittest.main()
