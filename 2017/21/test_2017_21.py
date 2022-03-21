#! /usr/bin/env python3

import unittest
import numpy as np
import puzzle_2017_21 as puz


class Test_2017_21(unittest.TestCase):
    def setUp(self):
        self.rules = puz.parse_rules("test_input.txt")

    def test_evolve(self):
        canvas = puz.evolve(puz.INITIAL_CANVAS, self.rules, t=1)
        self.assertEqual(canvas, "#..#/..../..../#..#")

        canvas_2 = puz.evolve(canvas, self.rules, t=1)
        self.assertEqual(canvas_2, "##.##./#..#../....../##.##./#..#../......")

    def test_count_on(self):
        canvas_2 = puz.evolve(puz.INITIAL_CANVAS, self.rules, t=2)
        self.assertEqual(puz.count_on(canvas_2), 12)

    def test_type_convert(self):
        m = np.array([[1, 0], [0, 1]], dtype=bool)
        s = "#./.#"
        self.assertEqual(puz.array_to_str(m), s)
        self.assertEqual(np.all(puz.str_to_array(s) == m), True)


if __name__ == "__main__":
    unittest.main()
