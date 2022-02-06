#! /usr/bin/env python3

import unittest
import puzzle_2016_06 as puz


class Test_2016_06(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()
        self.messages = puz.parse(data)

    def test_extract_message_a(self):
        self.assertEqual(puz.extract_message(self.messages, puz.max_extractor), "easter")

    def test_extract_message_b(self):
        self.assertEqual(puz.extract_message(self.messages, puz.min_extractor), "advent")


if __name__ == "__main__":
    unittest.main()
