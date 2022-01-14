#!/usr/bin/env python3

import unittest
from reindeer import *
from puzzle_2015_14b import scores_after


class ReindeerRacingTest(unittest.TestCase):
    def setUp(self):
        self.comet = Reindeer("Comet", 14, 10, 127)
        self.dancer = Reindeer("Dancer", 16, 11, 162)
        self.herd = [self.comet, self.dancer]

    def test_distance_travelled(self):
        self.assertEqual(self.comet.distance_travelled(1), 14)
        self.assertEqual(self.comet.distance_travelled(10), 140)
        self.assertEqual(self.comet.distance_travelled(1000), 1120)
        self.assertEqual(self.dancer.distance_travelled(1), 16)
        self.assertEqual(self.dancer.distance_travelled(11), 176)
        self.assertEqual(self.dancer.distance_travelled(1000), 1056)

    def test_scores_after(self):
        scores_1 = scores_after(self.herd, 1)
        scores_140 = scores_after(self.herd, 140)
        scores_1000 = scores_after(self.herd, 1000)

        self.assertEqual(scores_1["Comet"], 0)
        self.assertEqual(scores_140["Comet"], 1)
        self.assertEqual(scores_1000["Comet"], 312)
        self.assertEqual(scores_1["Dancer"], 1)
        self.assertEqual(scores_140["Dancer"], 139)
        self.assertEqual(scores_1000["Dancer"], 689)


if __name__ == "__main__":
    unittest.main()
