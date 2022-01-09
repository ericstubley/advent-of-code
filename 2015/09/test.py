#!/usr/bin/env python3

import unittest
from puzzle_2015_09a import parse_data, shortest_path
from puzzle_2015_09b import longest_path


class SantaPathFinder(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        strings = [__.rstrip('\n') for __ in data]
        self.places, self.distances = parse_data(strings)

    def test_shortest_path(self):
        self.assertEqual(shortest_path(self.places, self.distances), 605)

    def test_longest_path(self):
        self.assertEqual(longest_path(self.places, self.distances), 982)


if __name__ == "__main__":
    unittest.main()
