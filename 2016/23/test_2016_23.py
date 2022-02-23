#! /usr/bin/env python3

import unittest
import puzzle_2016_23 as puz


class Test_2016_23(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()
        self.instructions_test = puz.parse(data)
        with open("test_input_12.txt") as f:
            data = f.readlines()
        self.instructions_test_12 = puz.parse(data)
        with open("input_12.txt") as f:
            data = f.readlines()
        self.instructions_12 = puz.parse(data)

    def test_old_instructions(self):
        self.assertListEqual(puz.run_instructions(self.instructions_test_12, [0,0,0,0]), [42,0,0,0])
        self.assertListEqual(puz.run_instructions(self.instructions_12, [0,0,0,0]), [318009,196418,0,0])
        self.assertListEqual(puz.run_instructions(self.instructions_12, [0,0,1,0]), [9227663,5702887,0,0])

    def test_tgl_instructions(self):
        self.assertListEqual(puz.run_instructions(self.instructions_test, [0,0,0,0]), [3,0,0,0])
        self.assertEqual(self.instructions_test[3][0], "inc")
        self.assertEqual(self.instructions_test[4][0], "jnz")


if __name__ == "__main__":
    unittest.main()
