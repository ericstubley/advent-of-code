#!/usr/bin/env python3

import unittest
from puzzle_2015_08a import count_code_characters, count_memory_characters 
from puzzle_2015_08b import encode_string


class StringCountTest(unittest.TestCase):
    def test_count_code_characters(self):
        self.assertEqual(count_code_characters('""'), 2)
        self.assertEqual(count_code_characters('"abc"'), 5)
        self.assertEqual(count_code_characters('"aaa\\\"aaa"'), 10)
        self.assertEqual(count_code_characters('"\\x27"'), 6)

    def test_count_memory_characters(self):
        self.assertEqual(count_memory_characters('""'), 0)
        self.assertEqual(count_memory_characters('"abc"'), 3)
        self.assertEqual(count_memory_characters('"aaa\\\"aaa"'), 7)
        self.assertEqual(count_memory_characters('"\\x27"'), 1)

    def test_encode_string(self):
        self.assertEqual(encode_string('""'), '"\\\"\\\""')
        self.assertEqual(encode_string('"abc"'), '"\\\"abc\\\""')
        self.assertEqual(encode_string('"aaa\\\"aaa"'), '"\\\"aaa\\\\\\\"aaa\\\""')
        self.assertEqual(encode_string('"\\x27"'), '"\\\"\\\\x27\\\""')

    def test_encoded_character_count(self):
        self.assertEqual(count_code_characters(encode_string('""')), 6)
        self.assertEqual(count_code_characters(encode_string('"abc"')), 9)
        self.assertEqual(count_code_characters(encode_string('"aaa\\\"aaa"')), 16)
        self.assertEqual(count_code_characters(encode_string('"\\x27"')), 11)


if __name__ == "__main__":
    unittest.main()
