#! /usr/bin/env python3

import unittest
import puzzle_2017_12 as puz


class Test_2017_12(unittest.TestCase):
    def setUp(self):
        self.graph = puz.parse_pipe_graph("test_input.txt")

    def test_component_size(self):
        self.assertEqual(puz.component_size(self.graph, 0), 6)

    def test_count_components(self):
        self.assertEqual(puz.count_components(self.graph), 2)


if __name__ == "__main__":
    unittest.main()
