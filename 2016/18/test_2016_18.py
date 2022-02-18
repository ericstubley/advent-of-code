#! /usr/bin/env python3

import unittest
import bitarray as ba
import puzzle_2016_18 as puz


class Test_2016_18(unittest.TestCase):
    def test_count_safe_tiles(self):
        ba_1 = ba.bitarray([False, False, True, True, False])
        ba_2 = ba.bitarray(".^^.^.^^^^".replace('^', '1').replace('.', '0'))
        self.assertEqual(puz.count_safe_tiles(ba_1, 3), 6)
        self.assertEqual(puz.count_safe_tiles(ba_2, 10), 38)


if __name__ == "__main__":
    unittest.main()
