#! /usr/bin/env python3

import unittest
import puzzle_2017_14 as puz


class Test_2017_14(unittest.TestCase):
    def test_disk_usage(self):
        self.assertEqual(puz.disk_usage("flqrgnkx"), 8108)

    def test_row_usage(self):
        self.assertTrue(puz.row_digits("flqrgnkx", 0).startswith("d4"))
        self.assertTrue(puz.row_digits("flqrgnkx", 1).startswith("55"))
        self.assertTrue(puz.row_digits("flqrgnkx", 2).startswith("0a"))
        self.assertTrue(puz.row_digits("flqrgnkx", 3).startswith("ad"))
        self.assertTrue(puz.row_digits("flqrgnkx", 4).startswith("68"))
        self.assertTrue(puz.row_digits("flqrgnkx", 5).startswith("c9"))
        self.assertTrue(puz.row_digits("flqrgnkx", 6).startswith("44"))
        self.assertTrue(puz.row_digits("flqrgnkx", 7).startswith("d6"))

    def test_count_regions(self):
        grid, __ = puz.build_grid("flqrgnkx")
        puz.print_grid(grid)
        self.assertEqual(puz.count_regions("flqrgnkx"), 1242)


if __name__ == "__main__":
    unittest.main()
