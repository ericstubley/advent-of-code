#! /usr/bin/env python3

import unittest
import puzzle_2016_15 as puz


class Test_2016_15(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.congruences = puz.parse_congruences(data)

    def test_parse_congruences(self):
        self.assertEqual(self.congruences[0], (5, 5))
        self.assertEqual(self.congruences[1], (2, 3))

    def test_crt(self):
        self.assertEqual(puz.crt(self.congruences), 5)


if __name__ == "__main__":
    unittest.main()
