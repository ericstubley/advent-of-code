#! /usr/bin/env python3

import unittest
import puzzle_2016_03 as puz


class Test_2016_03(unittest.TestCase):
    def test_parse(self):
        with open("input.txt") as f:
            data = f.readlines()
        triangles = puz.parse(data)
        self.assertTupleEqual(triangles[0], (541, 588, 421))

    def test_is_valid_triangle(self):
        self.assertEqual(puz.is_valid_triangle((5, 10, 25)), False)
        self.assertEqual(puz.is_valid_triangle((3, 4, 5)), True)


    def test_column_parse(self):
        with open("test_input.txt") as f:
            data = f.readlines()
        triangles = puz.column_parse(data)
        self.assertEqual(len([t for t in triangles if puz.is_valid_triangle(t)]), 6)


if __name__ == "__main__":
    unittest.main()
