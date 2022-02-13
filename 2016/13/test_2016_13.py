#! /usr/bin/env python3

import unittest
import puzzle_2016_13 as puz


class Test_2016_13(unittest.TestCase):
    def setUp(self):
        self.favourite = 10
        self.goal = (7, 4)

    def test_is_wall(self):
        self.assertEqual(puz.is_wall(0, 0, self.favourite), False)
        self.assertEqual(puz.is_wall(1, 0, self.favourite), True)
        self.assertEqual(puz.is_wall(2, 0, self.favourite), False)
        self.assertEqual(puz.is_wall(0, 1, self.favourite), False)
        self.assertEqual(puz.is_wall(1, 1, self.favourite), False)
        self.assertEqual(puz.is_wall(2, 1, self.favourite), True)
        self.assertEqual(puz.is_wall(0, 2, self.favourite), True)
        self.assertEqual(puz.is_wall(1, 2, self.favourite), False)
        self.assertEqual(puz.is_wall(2, 2, self.favourite), False)

    def test_min_steps(self):
        self.assertEqual(puz.min_steps(3, 1, self.favourite), 4) 
        self.assertEqual(puz.min_steps(7, 4, self.favourite), 11) 

    def test_locations_found(self):
        self.assertEqual(puz.locations_found(0, self.favourite), 1)
        self.assertEqual(puz.locations_found(1, self.favourite), 3)
        self.assertEqual(puz.locations_found(2, self.favourite), 5)
        self.assertEqual(puz.locations_found(3, self.favourite), 6)


if __name__ == "__main__":
    unittest.main()
