#!/usr/bin/env python3

import unittest
import puzzle_2015_24a as a
import puzzle_2015_24b as b


class SleighPackerTest(unittest.TestCase):
    def setUp(self):
        self.test_weights = (1, 2, 3, 4, 5, 7, 8, 9, 10, 11)

    def test_ideal_first_group(self):
        self.assertTupleEqual(a.ideal_first_group(self.test_weights), (9, 11))


if __name__ == "__main__":
    unittest.main()
