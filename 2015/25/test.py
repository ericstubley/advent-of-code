#!/usr/bin/env python3

import unittest
import puzzle_2015_25a as a


class WeatherMachineCodeTest(unittest.TestCase):
    def test_code_by_index(self):
        self.assertEqual(a.code_by_index(8), 8057251)
        self.assertEqual(a.code_by_index(18), 21345942)

    def test_code_by_grid(self):
        self.assertEqual(a.code_by_grid(3, 2), 8057251)
        self.assertEqual(a.code_by_grid(4, 3), 21345942)


if __name__ == "__main__":
    unittest.main()
    