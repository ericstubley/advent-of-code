#! /usr/bin/env python3

import unittest
import puzzle_2017_20 as puz


class Test_2017_20(unittest.TestCase):
    def setUp(self):
        self.data_a = puz.parse_data("test_input_a.txt")
        self.data_b = puz.parse_data("test_input_b.txt")

    def test_closest_long_term(self):
        self.assertEqual(puz.closest_long_term(self.data_a), 0)

    def test_long_term_count(self):
        self.assertEqual(puz.long_term_count(self.data_b), 1)


if __name__ == "__main__":
    unittest.main()
