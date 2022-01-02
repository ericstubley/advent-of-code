#!/usr/bin/env python3

import unittest
from puzzle_2015_02a import paper_required
from puzzle_2015_02b import ribbon_required

class WrappingPaperTest(unittest.TestCase):
    def test_paper_required(self):
        self.assertEqual(paper_required(2, 3, 4), 58)
        self.assertEqual(paper_required(1, 1, 10), 43)

    def test_ribbon_required(self):
        self.assertEqual(ribbon_required(2, 3, 4), 34)
        self.assertEqual(ribbon_required(1, 1, 10), 14)

if __name__ == "__main__":
    unittest.main()