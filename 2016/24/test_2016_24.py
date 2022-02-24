#! /usr/bin/env python3

import unittest
import puzzle_2016_24 as puz


class Test_2016_24(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.nodes, self.maze = puz.parse(data)
        self.roads = puz.shortest_roads(self.nodes, self.maze)


    def test_shortest_roads(self):
        self.assertEqual(self.roads[(0, 1)], 2)
        self.assertEqual(self.roads[(0, 2)], 8)
        self.assertEqual(self.roads[(0, 3)], 10)
        self.assertEqual(self.roads[(0, 4)], 2)
        self.assertEqual(self.roads[(1, 2)], 6)
        self.assertEqual(self.roads[(1, 3)], 8)
        self.assertEqual(self.roads[(1, 4)], 4)
        self.assertEqual(self.roads[(2, 3)], 2)
        self.assertEqual(self.roads[(2, 4)], 10)
        self.assertEqual(self.roads[(3, 4)], 8)


    def test_shortest_path(self):
        self.assertEqual(puz.shortest_path(len(self.nodes), self.roads)[0], 14)
        print(puz.shortest_path(len(self.nodes), self.roads))


if __name__ == "__main__":
    unittest.main()
