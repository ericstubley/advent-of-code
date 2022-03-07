#! /usr/bin/env python3

import unittest
import puzzle_2017_07 as puz


class Test_2017_07(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.tower = puz.parse_data(data)

    def test_bottom(self):
        self.assertEqual(puz.bottom(self.tower), "tknk")

    def test_find_incorrect(self):
        self.assertEqual(puz.find_incorrect(self.tower), "ugml")

    def test_correct_weight(self):
        self.assertEqual(puz.correct_weight(self.tower), 60)


if __name__ == "__main__":
    unittest.main()
