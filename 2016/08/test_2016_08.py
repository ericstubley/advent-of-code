#! /usr/bin/env python3

import unittest
import numpy as np
import puzzle_2016_08 as puz


class Test_2016_08(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines() 

        self.instructions = puz.parse(data)
        self.dimensions = (3, 7)
        self.display = np.zeros(self.dimensions, dtype=bool)

    def test_operations(self):
        puz.operation(self.instructions[0], self.display)
        result_0 = np.array(
            [[True, True, True, False, False, False, False],
            [True, True, True, False, False, False, False],
            [False, False, False, False, False, False, False]], dtype=bool)
        self.assertTrue(np.array_equal(self.display, result_0))

        puz.operation(self.instructions[1], self.display)
        result_1 = np.array(
            [[True, False, True, False, False, False, False],
            [True, True, True, False, False, False, False],
            [False, True, False, False, False, False, False]], dtype=bool)
        self.assertTrue(np.array_equal(self.display, result_1))

        puz.operation(self.instructions[2], self.display)
        result_2 = np.array(
            [[False, False, False, False, True, False, True],
            [True, True, True, False, False, False, False],
            [False, True, False, False, False, False, False]], dtype=bool)
        self.assertTrue(np.array_equal(self.display, result_2))

        puz.operation(self.instructions[3], self.display)
        result_3 = np.array(
            [[False, True, False, False, True, False, True],
            [True, False, True, False, False, False, False],
            [False, True, False, False, False, False, False]], dtype=bool)
        self.assertTrue(np.array_equal(self.display, result_3))
         



if __name__ == "__main__":
    unittest.main()
