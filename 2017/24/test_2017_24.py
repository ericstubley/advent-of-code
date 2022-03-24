#! /usr/bin/env python3

import unittest
import puzzle_2017_24 as puz


class Test_2017_24(unittest.TestCase):
    def setUp(self):
        self.vertices, self.edges = puz.parse_graph("test_input.txt")

    def test_maximum_strength(self):
        self.assertEqual(puz.maximum_strength(self.vertices, self.edges), 31)


    def test_maximum_length(self):
        self.assertEqual(puz.maximum_length(self.vertices, self.edges), (4, 19)) 


if __name__ == "__main__":
    unittest.main()
