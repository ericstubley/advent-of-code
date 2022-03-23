#! /usr/bin/env python3

import unittest
import puzzle_2017_23 as puz


class Test_2017_23(unittest.TestCase):
    def test_is_prime(self):
        self.assertEqual(puz.is_prime(2), True)
        self.assertEqual(puz.is_prime(3), True)
        self.assertEqual(puz.is_prime(5), True)
        self.assertEqual(puz.is_prime(7), True)
        self.assertEqual(puz.is_prime(101), True)
        self.assertEqual(puz.is_prime(4), False)
        self.assertEqual(puz.is_prime(57), False)

    def test_count_composite(self):
        self.assertEqual(puz.count_composite(3, 11, 2), 1)
        self.assertEqual(puz.count_composite(3, 11, 1), 5)


if __name__ == "__main__":
    unittest.main()
