#!/usr/bin/env python3

import unittest
import puzzle_2015_18a as a
import puzzle_2015_18b as b


class LightGridTest(unittest.TestCase):
    def setUp(self):
        self.test_grid = a.grid_from_file("test_input.txt")
        self.test_grid_b = b.grid_from_file("test_input.txt")

    def test_update_grid(self):
        for i in range(4):
            self.test_grid = a.update_grid(self.test_grid)
            
        self.assertEqual(self.test_grid.sum(), 4) 

    def test_update_grid_b(self):
        for i in range(5):
            self.test_grid_b = b.update_grid(self.test_grid_b)
            
        self.assertEqual(self.test_grid_b.sum(), 17) 



if __name__ == "__main__":
    unittest.main()
