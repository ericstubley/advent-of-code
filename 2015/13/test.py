#!/usr/bin/env python3

import unittest
import puzzle_2015_13a as a


class SeatingTest(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.preferences = a.process_input(data)

    def test_happiness(self):
        self.assertEqual(a.happiness(list(self.preferences.keys()), self.preferences), 330)

    def test_find_optimal(self):
        self.assertEqual(a.find_optimal(self.preferences), 330)


if __name__ == "__main__":
    unittest.main()
