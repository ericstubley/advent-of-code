#!/usr/bin/env python3

import unittest
from puzzle_2015_03a import next_position, houses_visited
from puzzle_2015_03b import split_houses_visited

class PresentDeliveryTest(unittest.TestCase):
    def test_next_position(self):
        # this test feels superfluous
        # there's certainly an art to writing good tests that you haven't
        # digested yet
        self.assertEqual(next_position((0, 0), '>'), (1, 0))

    def test_houses_visited(self):
        self.assertEqual(houses_visited(">"), 2)
        self.assertEqual(houses_visited("^>v<"), 4)
        self.assertEqual(houses_visited("^v^v^v^v^v"), 2)

    def test_split_houses_visited(self):
        self.assertEqual(split_houses_visited("^v"), 3)
        self.assertEqual(split_houses_visited("^>v<"), 3)
        self.assertEqual(split_houses_visited("^v^v^v^v^v"), 11)

if __name__ == "__main__":
    unittest.main()