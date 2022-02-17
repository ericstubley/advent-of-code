#! /usr/bin/env python3

import unittest
import puzzle_2016_17 as puz


class Test_2016_17(unittest.TestCase):

    def test_path_to_position(self):
        self.assertEqual(puz.path_to_position(""), (0, 0))
        self.assertEqual(puz.path_to_position("DDURRL"), (1, 1))
        self.assertEqual(puz.path_to_position("DDDRRR"), (3, 3))


    def test_open_paths(self):
        self.assertEqual(puz.open_paths("hijkl", ""), ["D"])
        self.assertEqual(puz.open_paths("hijkl", "D"), ["U", "R"])
        self.assertEqual(puz.open_paths("hijkl", "DR"), [])
        self.assertEqual(puz.open_paths("hijkl", "DU"), ["R"])
        self.assertEqual(puz.open_paths("hijkl", "DUR"), [])


    def test_shortest_path(self):
        self.assertEqual(puz.find_paths("ihgpwlah", shortest_only=True), "DDRRRD")
        self.assertEqual(puz.find_paths("kglvqrro", shortest_only=True), "DDUDRLRRUDRD")
        self.assertEqual(puz.find_paths("ulqzkmiv", shortest_only=True), "DRURDRUDDLLDLUURRDULRLDUUDDDRR")


    def test_longest_path(self):
        self.assertEqual(len(puz.longest_path("ihgpwlah")), 370)
        self.assertEqual(len(puz.longest_path("kglvqrro")), 492)
        self.assertEqual(len(puz.longest_path("ulqzkmiv")), 830)


if __name__ == "__main__":
    unittest.main()
