#!/usr/bin/env python3

import unittest
from puzzle_2015_01a import final_floor
from puzzle_2015_01b import first_basement_visit


class ElevatorTest(unittest.TestCase):
    def test_final_floor(self):
        self.assertEqual(final_floor("(())"), 0)
        self.assertEqual(final_floor("()()"), 0)
        self.assertEqual(final_floor("(()(()("), 3)
        self.assertEqual(final_floor("))((((("), 3)
        self.assertEqual(final_floor("())"), -1)
        self.assertEqual(final_floor("))("), -1)
        self.assertEqual(final_floor(")))"), -3)
        self.assertEqual(final_floor(")())())"), -3)

    def test_first_basement_visit(self):
        self.assertEqual(first_basement_visit(")"), 1)
        self.assertEqual(first_basement_visit("()())"), 5)


if __name__ == "__main__":
    unittest.main()