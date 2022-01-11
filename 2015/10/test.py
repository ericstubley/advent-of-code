#!/usr/bin/env python3

import unittest
from puzzle_2015_10a import look_and_say
from puzzle_2015_10b import look_and_say as look_and_say_b

class LookAndSayTest(unittest.TestCase):
    def test_look_and_say(self):
        self.assertEqual(look_and_say([1]), [1, 1])
        self.assertEqual(look_and_say([1, 1]), [2, 1])
        self.assertEqual(look_and_say([2, 1]), [1, 2, 1, 1])
        self.assertEqual(look_and_say([1, 2, 1, 1]), [1, 1, 1, 2, 2, 1])
        self.assertEqual(look_and_say([1, 1, 1, 2, 2, 1]), [3, 1, 2, 2, 1, 1])

    def test_look_and_say_b(self):
        self.assertEqual(look_and_say_b([1]), [1, 1])
        self.assertEqual(look_and_say_b([1, 1]), [2, 1])
        self.assertEqual(look_and_say_b([2, 1]), [1, 2, 1, 1])
        self.assertEqual(look_and_say_b([1, 2, 1, 1]), [1, 1, 1, 2, 2, 1])
        self.assertEqual(look_and_say_b([1, 1, 1, 2, 2, 1]), [3, 1, 2, 2, 1, 1])

if __name__ == "__main__":
    unittest.main()