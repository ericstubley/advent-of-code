#!/usr/bin/env python3

import unittest
import puzzle_2015_23a as a
import puzzle_2015_23b as b

class TuringLockTest(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            test_instructions = f.readlines()
        self.instructions = a.parse_instructions(test_instructions)

    def test_program(self):
        self.assertTupleEqual(a.run_program(self.instructions), (2, 0))


if __name__ == "__main__":
    unittest.main()