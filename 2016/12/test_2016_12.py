#! /usr/bin/env python3

import unittest
import puzzle_2016_12 as puz


class Test_2016_12(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.instructions = puz.parse(data)

    def test_run_instructions(self):
        self.assertListEqual(puz.run_instructions(self.instructions, [0, 0, 0, 0]), [42, 0, 0, 0])


if __name__ == "__main__":
    unittest.main()
