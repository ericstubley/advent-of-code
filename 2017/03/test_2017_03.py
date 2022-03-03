#! /usr/bin/env python3

import unittest
import puzzle_2017_03 as puz


class Test_2017_03(unittest.TestCase):
    def test_spiral(self):
        for i in range(1, 26):
            print(puz.ulam_coordinates(i))


    def test_ulam_coordinates(self):
        self.assertEqual(puz.ulam_coordinates(1), (0, 0))
        self.assertEqual(puz.ulam_coordinates(12), (2, 1))
        self.assertEqual(puz.ulam_coordinates(23), (0, -2))
        self.assertEqual(puz.ulam_coordinates(1024), (-15, 16))


    def test_manhattan_distance(self):
        self.assertEqual(puz.ulam_in_ny(1), 0)
        self.assertEqual(puz.ulam_in_ny(12), 3)
        self.assertEqual(puz.ulam_in_ny(23), 2)
        self.assertEqual(puz.ulam_in_ny(1024), 31)


    def test_ulam_lies(self):
        self.assertEqual(puz.ulam_lies(12), 23)
        self.assertEqual(puz.ulam_lies(747), 747)
        self.assertEqual(puz.ulam_lies(748), 806)


if __name__ == "__main__":
    unittest.main()
