#! /usr/bin/env python3

import unittest
import puzzle_2017_08 as puz


class Test_2017_08(unittest.TestCase):
    def setUp(self):
        self.instructions = puz.parse_instructions("test_input.txt")


    def test_max_register(self):
        self.assertEqual(puz.max_register(self.instructions), 1)


    def test_run_instructions(self):
        self.assertEqual(puz.run_instructions(self.instructions), {'a': 1, 'b': 0, 'c': -10})


    def test_max_ever(self):
        registers, highest = puz.run_instructions(self.instructions, report=True)
        self.assertEqual(highest, 10)


if __name__ == "__main__":
    unittest.main()
