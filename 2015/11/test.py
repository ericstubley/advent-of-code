#!/usr/bin/env python3

import unittest
from puzzle_2015_11a import is_valid_password, next_valid, next_string

class PasswordTester(unittest.TestCase):
    def test_is_valid_password(self):
        self.assertFalse(is_valid_password("hijklmmn"))
        self.assertFalse(is_valid_password("abbceffg"))
        self.assertFalse(is_valid_password("abbcegjk"))

    def test_next_valid(self):
        self.assertEqual(next_valid("abcdefgh"), "abcdffaa")
        self.assertEqual(next_valid("ghijklmn"), "ghjaabcc")

    def test_next_string(self):
        self.assertEqual(next_string("xy"), "xz")
        self.assertEqual(next_string("xz"), "ya")


if __name__ == "__main__":
    unittest.main()
