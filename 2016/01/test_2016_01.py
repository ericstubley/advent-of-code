#! /usr/bin/env python3

import unittest
import puzzle_2016_01 as puz


class Test_2016_01(unittest.TestCase):
    def setUp(self):
        self.instructions_1 = puz.parse_instructions("R2, L3")
        self.instructions_2 = puz.parse_instructions("R2, R2, R2")
        self.instructions_3 = puz.parse_instructions("R5, L5, R5, R3")
        self.instructions_4 = puz.parse_instructions("R8, R4, R4, R8")

    def test_total_distance(self):
        self.assertEqual(puz.total_distance(self.instructions_1), 5)
        self.assertEqual(puz.total_distance(self.instructions_2), 2)
        self.assertEqual(puz.total_distance(self.instructions_3), 12)

    def test_first_visit_twice(self):
        self.assertEqual(puz.first_visit_twice(self.instructions_4), 4)


if __name__ == "__main__":
    unittest.main()
