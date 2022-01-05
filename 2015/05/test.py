#!/usr/bin/env python3

import unittest
import puzzle_2015_05a as a
import puzzle_2015_05b as b


class NiceStringTest(unittest.TestCase):
    def test_is_nice_string_a(self):
        self.assertTrue(a.is_nice_string("ugknbfddgicrmopn"))
        self.assertTrue(a.is_nice_string("aaa"))
        self.assertFalse(a.is_nice_string("jchzalrnumimnmhp"))
        self.assertFalse(a.is_nice_string("haegwjzuvuyypxyu"))
        self.assertFalse(a.is_nice_string("dvszwmarrgswjxmb"))

    def test_has_three_vowels(self):
        self.assertTrue(a.has_three_vowels("aei"))
        self.assertTrue(a.has_three_vowels("xazegov"))
        self.assertTrue(a.has_three_vowels("aeiouaeiouaeiou"))
        self.assertFalse(a.has_three_vowels("dvszwmarrgswjxmb"))

    def test_has_double_letter(self):
        self.assertTrue(a.has_double_letter("xx"))
        self.assertTrue(a.has_double_letter("abcdde"))
        self.assertTrue(a.has_double_letter("aabbccdd"))
        self.assertFalse(a.has_double_letter("jchzalrnumimnmhp"))

    def test_has_banned_substring(self):
        self.assertFalse(a.has_banned_substring("ugknbfddgicrmopn"))
        self.assertTrue(a.has_banned_substring("haegwjzuvuyypxyu"))

    def test_is_nice_string_b(self):
        self.assertTrue(b.is_nice_string("qjhvhtzxzqqjkmpb"))
        self.assertTrue(b.is_nice_string("xxyxx"))
        self.assertFalse(b.is_nice_string("uurcxstgmygtbstg"))
        self.assertFalse(b.is_nice_string("ieodomkazucvgmuy"))

    def test_pair_appears_twice(self):
        self.assertTrue(b.pair_appears_twice("xyxy"))
        self.assertTrue(b.pair_appears_twice("aabcdefgaa"))
        self.assertFalse(b.pair_appears_twice("aaa"))

    def test_has_xyx_pattern(self):
        self.assertTrue(b.has_xyx_pattern("xyx"))
        self.assertTrue(b.has_xyx_pattern("abcdefeghi"))
        self.assertTrue(b.has_xyx_pattern("aaa"))
        self.assertFalse(b.has_xyx_pattern("uurcxstgmygtbstg"))


if __name__ == "__main__":
    unittest.main()
