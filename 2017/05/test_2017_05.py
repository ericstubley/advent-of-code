#! /usr/bin/env python3

import unittest
import puzzle_2017_05 as puz


class Test_2017_05(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            self.rows = [int(line.rstrip()) for line in f.readlines()]
        self.rows_strange = [__ for __ in self.rows]

    def test_count_jumps(self):
        self.assertEqual(puz.count_jumps(self.rows), 5)
        self.assertListEqual(self.rows, [2, 5, 0, 1, -2])

        self.assertEqual(puz.count_jumps(self.rows_strange, strange=True), 10)
        self.assertListEqual(self.rows_strange, [2, 3, 2, 3, -1])

if __name__ == "__main__":
    unittest.main()
