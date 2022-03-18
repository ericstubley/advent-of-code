#! /usr/bin/env python3

import unittest
import puzzle_2017_18 as puz


class Test_2017_18(unittest.TestCase):
    def test_first_recover(self):
        self.instructions = puz.parse_instructions("test_input_a.txt")
        self.assertEqual(puz.first_recover(self.instructions), 4)

    def test_number_of_sends(self):
        self.instructions = puz.parse_instructions("test_input_b.txt")
        self.assertEqual(puz.number_of_sends(self.instructions), 3)


if __name__ == "__main__":
    unittest.main()
