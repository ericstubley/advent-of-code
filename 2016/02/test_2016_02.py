#! /usr/bin/env python3

import unittest
import puzzle_2016_02 as puz


class Test_2016_02(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.instructions = puz.parse(data)

    def test_get_code_a(self):
        self.assertEqual(puz.get_code_from_keypad(self.instructions, puz.KEYPAD_A), "1985")

    def test_get_code_b(self):
        self.assertEqual(puz.get_code_from_keypad(self.instructions, puz.KEYPAD_B), "5DB3")


if __name__ == "__main__":
    unittest.main()
