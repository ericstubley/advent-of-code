#!/usr/bin/env python3

import unittest
import numpy as np

import puzzle_2015_06a as a
import puzzle_2015_06b as b


class LightInstructionsTest(unittest.TestCase):
    def test_get_range_from_text(self):
        self.assertEqual(a.get_range_from_text("887,9 through 959,629"), (887, 9, 960, 630))

    def test_part_b_instructions(self):
        test_array = np.ones((2, 2), dtype=int)
        zero_array = np.zeros((2, 2), dtype=int)

        self.assertIsNone(np.testing.assert_array_equal(b.turn_on(test_array), 2*test_array))
        self.assertIsNone(np.testing.assert_array_equal(b.toggle(test_array), 3*test_array))
        self.assertIsNone(np.testing.assert_array_equal(b.turn_off(test_array), zero_array))
        self.assertIsNone(np.testing.assert_array_equal(b.turn_off(zero_array), zero_array))


if __name__ == "__main__":
    unittest.main()