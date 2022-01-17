#!/usr/bin/env python3

import unittest
import puzzle_2015_17a as a
import puzzle_2015_17b as b


class EggnogTest(unittest.TestCase):
    def setUp(self):
        self.target = 25
        self.containers = (20, 15, 10, 5, 5)

    def test_number_of_configurations(self):
        self.assertEqual(a.number_of_configurations(self.target, self.containers), 4)

    def test_container_stats(self):
        cs = b.container_stats(self.target, self.containers)
        test = [[5, 20], [5, 20], [10, 15], [5, 5, 15]]


        self.assertListEqual(b.container_stats(10, (10, 10)), [[10], [10]])
        self.assertListEqual(cs, test)


if __name__ == "__main__":
    unittest.main()
