#! /usr/bin/env python3

import unittest
import puzzle_2017_22 as puz


class Test_2017_22(unittest.TestCase):
    def setUp(self):
        self.grid = puz.parse_grid("test_input.txt")

    def test_burst(self):
        x, y, d, infect = puz.burst(self.grid, 4, 4, puz.Direction.U)

    def test_infect_count_short(self):
        x, y, d, count = puz.evolve(self.grid, 4, 4, 70)
        self.assertEqual(count, 41)

    def test_infect_count_long(self):
        x, y, d, count = puz.evolve(self.grid, 4, 4, 10000)
        self.assertEqual(count, 5587)

    def test_infect_count_evolved_short(self):
        x, y, d, count = puz.evolve(self.grid, 4, 4, 100, intelligent=True)
        self.assertEqual(count, 26)

    def test_infect_count_evolved_long(self):
        x, y, d, count = puz.evolve(self.grid, 4, 4, 10000000, intelligent=True)
        self.assertEqual(count, 2511944)



if __name__ == "__main__":
    unittest.main()
