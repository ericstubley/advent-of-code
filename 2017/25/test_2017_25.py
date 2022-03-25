#! /usr/bin/env python3

import unittest
import puzzle_2017_25 as puz


class Test_2017_25(unittest.TestCase):
    def setUp(self):
        self.tm = puz.parse_turing_machine("test_input.txt")

    def test_checksum_diagnostic(self):
        self.assertEqual(self.tm.diagnostic_checksum(), 3)


if __name__ == "__main__":
    unittest.main()
