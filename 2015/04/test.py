#!/usr/bin/env python3

import unittest
from puzzle_2015_04a import get_md5_prefix, find_lowest_for_prefix

class AdventCoinTest(unittest.TestCase):
    def test_get_md5_prefix(self):
        self.assertEqual(get_md5_prefix(to_hash="abcdef609043", length=5), "00000")
        self.assertEqual(get_md5_prefix(to_hash="abcdef609143", length=5), "ba9c6")

    def test_find_lowest_for_prefix(self):
        self.assertEqual(find_lowest_for_prefix(key="abcdef", prefix="00000"), 609043)
        self.assertEqual(find_lowest_for_prefix(key="pqrstuv", prefix="00000"), 1048970)

if __name__ == "__main__":
    unittest.main()