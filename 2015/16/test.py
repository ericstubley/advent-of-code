#!/usr/bin/env python3

import unittest
from aunt import *
import puzzle_2015_16a as a
import puzzle_2015_16b as b


class AuntTest(unittest.TestCase):
    def setUp(self):
        self.fake_aunt_a = Aunt("Sue 0: cars: 2, trees: 4")
        self.fake_aunt_b = Aunt("Sue 0: cars: 2, goldfish: 0")
        self.fake_aunt_c = Aunt("Sue 0: cars: 2, goldfish: 5")
        self.aunt_1 = Aunt("Sue 1: cars: 9, akitas: 3, goldfish: 0")

    def test_aunt_parsing(self):
        test_dict = {"cars": 9, "akitas": 3, "goldfish": 0}
        self.assertDictEqual(self.aunt_1.attributes, test_dict)

    def test_is_viable_a(self):
        # self.assertTrue(a.is_viable(self.fake_aunt))
        self.assertFalse(a.is_viable(self.aunt_1))

    def test_is_viable_b(self):
        self.assertTrue(b.is_viable(self.fake_aunt_a))
        self.assertTrue(b.is_viable(self.fake_aunt_b))
        self.assertFalse(b.is_viable(self.fake_aunt_c))
        self.assertFalse(b.is_viable(self.aunt_1))



if __name__ == "__main__":
    unittest.main()
