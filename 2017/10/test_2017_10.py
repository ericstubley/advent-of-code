#! /usr/bin/env python3

import unittest
import puzzle_2017_10 as puz


class Test_2017_10(unittest.TestCase):
    def setUp(self):
        self.salt = [17, 31, 73, 47, 23]


    def test_tie(self):
        self.assertEqual(puz.tie([0, 1, 2, 3, 4], 0, 3), [2, 1, 0, 3, 4])
        self.assertEqual(puz.tie([2, 1, 0, 3, 4], 3, 4), [4, 3, 0, 1, 2])
        self.assertEqual(puz.tie([4, 3, 0, 1, 2], 3, 1), [4, 3, 0, 1, 2])
        self.assertEqual(puz.tie([4, 3, 0, 1, 2], 1, 5), [3, 4, 2, 1, 0])

    def test_hash(self):
        self.assertEqual(puz.hash([0, 1, 2, 3, 4], [3, 4, 1, 5], 0, 0), ([3, 4, 2, 1, 0], 4, 4))

    def test_dense_hash(self):
        self.assertEqual(puz.dense_hash("", self.salt), "a2582a3a0e66e6e86e3812dcb672a272")
        self.assertEqual(puz.dense_hash("AoC 2017", self.salt), "33efeb34ea91902bb2f59c9920caa6cd")
        self.assertEqual(puz.dense_hash("1,2,3", self.salt), "3efbe78a8d82f29979031a4aa0b16a9d")
        self.assertEqual(puz.dense_hash("1,2,4", self.salt), "63960835bcdc130f0b66d7ff4f6a5a8e")


if __name__ == "__main__":
    unittest.main()
