#! /usr/bin/env python3

import unittest
import puzzle_2017_19 as puz


class Test_2017_19(unittest.TestCase):
    def setUp(self):
        self.path = puz.parse_path("test_input.txt")

    def test_walk_path(self):
        self.assertEqual(puz.walk_path(self.path), ("ABCDEF", 38))

    def test_find_start(self):
        self.assertEqual(puz.find_start(self.path), (0, 5))

    def test_move(self):
        self.assertEqual(puz.move(self.path, 0, 5, puz.Direction.D), (1, 5, puz.Direction.D))
        self.assertEqual(puz.move(self.path, 4, 5, puz.Direction.D), (5, 5, puz.Direction.R))
        self.assertEqual(puz.move(self.path, 5, 5, puz.Direction.R), (5, 6, puz.Direction.R))


if __name__ == "__main__":
    unittest.main()
