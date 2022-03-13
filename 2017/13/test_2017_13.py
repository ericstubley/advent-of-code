#! /usr/bin/env python3

import unittest
import puzzle_2017_13 as puz


class Test_2017_13(unittest.TestCase):
    def setUp(self):
        self.ranges = puz.parse_input_to_ranges("test_input.txt")

    def test_trip_severity(self):
        self.assertEqual(puz.trip_severity(self.ranges), 24)

    def test_caught(self):
        self.assertEqual(puz.caught(self.ranges, time=0), True)
        self.assertEqual(puz.caught(self.ranges, time=1), False)
        self.assertEqual(puz.caught(self.ranges, time=2), False)
        self.assertEqual(puz.caught(self.ranges, time=3), False)
        self.assertEqual(puz.caught(self.ranges, time=4), False)
        self.assertEqual(puz.caught(self.ranges, time=5), False)
        self.assertEqual(puz.caught(self.ranges, time=6), True)

    def test_optimal_delay(self):
        self.assertEqual(puz.optimal_delay(self.ranges), 10)


if __name__ == "__main__":
    unittest.main()
