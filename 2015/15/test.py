#!/usr/bin/env python3

import unittest
from cookie import *
import puzzle_2015_15a as a
import puzzle_2015_15b as b


class CookieTest(unittest.TestCase):
    def setUp(self):
        self.ingredients = []
        with open("test_input.txt") as f:
            data = f.readlines()

        for line in data:
            self.ingredients.append(a.process_ingredient(line))


    def test_maximize_score(self):
        self.assertEqual(a.maximize_score(self.ingredients), 62842880)

    def test_maximize_score_with_calories(self):
        self.assertEqual(b.maximize_score_with_calories(self.ingredients), 57600000)


if __name__ == "__main__":
    unittest.main()