#! /usr/bin/env python3

import unittest
import puzzle_2017_17 as puz


class Test_2017_17(unittest.TestCase):
    def test_hurricane(self):
        self.assertEqual(puz.hurricane(1, 3), [0, 1])
        self.assertEqual(puz.hurricane(2, 3), [0, 2, 1])
        self.assertEqual(puz.hurricane(3, 3), [0, 2, 3, 1])
        self.assertEqual(puz.hurricane(4, 3), [0, 2, 4, 3, 1])
        self.assertEqual(puz.hurricane(5, 3), [0, 5, 2, 4, 3, 1])
        self.assertEqual(puz.hurricane(6, 3), [0, 5, 2, 4, 3, 6, 1])


    def test_after(self):
        final_list = puz.hurricane(time_limit=2017, step=3)
        self.assertEqual(puz.element_after(final_list, 2017), 638)


    def test_virtual_hurricane(self):
        self.assertEqual(puz.virtual_hurricane(1, 3), 1)
        self.assertEqual(puz.virtual_hurricane(2, 3), 2)
        self.assertEqual(puz.virtual_hurricane(3, 3), 2)
        self.assertEqual(puz.virtual_hurricane(4, 3), 2)
        self.assertEqual(puz.virtual_hurricane(5, 3), 5)
        self.assertEqual(puz.virtual_hurricane(6, 3), 5)


if __name__ == "__main__":
    unittest.main()
