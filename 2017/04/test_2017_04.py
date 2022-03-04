#! /usr/bin/env python3

import unittest
import puzzle_2017_04 as puz


class Test_2017_04(unittest.TestCase):
    def test_is_valid(self):
        self.assertEqual(puz.is_valid_passphrase("aa bb cc dd ee".split()), True)
        self.assertEqual(puz.is_valid_passphrase("aa bb cc dd aa".split()), False)
        self.assertEqual(puz.is_valid_passphrase("aa bb cc dd aaa".split()), True)

    def test_is_anagram_valid(self):
        ap1 = map(puz.word_to_letter_count, "abcde fghij".split())
        ap2 = map(puz.word_to_letter_count, "abcde xyz ecdab".split())
        ap3 = map(puz.word_to_letter_count, "a ab abc abd abf abj".split())
        ap4 = map(puz.word_to_letter_count, "iiii oiii ooii oooi oooo".split())
        ap5 = map(puz.word_to_letter_count, "oiii ioii iioi iiio".split())
        self.assertEqual(puz.is_valid_passphrase(ap1), True)
        self.assertEqual(puz.is_valid_passphrase(ap2), False)
        self.assertEqual(puz.is_valid_passphrase(ap3), True)
        self.assertEqual(puz.is_valid_passphrase(ap4), True)
        self.assertEqual(puz.is_valid_passphrase(ap5), False)


if __name__ == "__main__":
    unittest.main()
