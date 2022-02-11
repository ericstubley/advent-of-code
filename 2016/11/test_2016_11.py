#! /usr/bin/env python3

import unittest
import puzzle_2016_11 as puz


class Test_2016_11(unittest.TestCase):
    def setUp(self):
        self.final_state = puz.GameState(elevator=4, n=2, chips=(4, 4), generators=(4, 4))
        self.start_state = puz.GameState(elevator=1, n=2, chips=(1, 1), generators=(2, 3))


    def test_is_valid(self):
        self.assertTrue(puz.is_valid(self.final_state))
        self.assertTrue(puz.is_valid(self.start_state))
        bad_state = puz.GameState(elevator=1, n=2, chips=(1, 2), generators=(2, 1))
        self.assertFalse(puz.is_valid(bad_state))


    def test_valid_moves(self):
        moves_from_final = [puz.GameState(elevator=3, n=2, chips=(3, 4), generators=(4, 4)),
                puz.GameState(elevator=3, n=2, chips=(4, 3), generators=(4, 4)),
                puz.GameState(elevator=3, n=2, chips=(3, 3), generators=(4, 4)),
                puz.GameState(elevator=3, n=2, chips=(4, 4), generators=(3, 3)),
                puz.GameState(elevator=3, n=2, chips=(3, 4), generators=(3, 4)),
                puz.GameState(elevator=3, n=2, chips=(4, 3), generators=(4, 3))]

        self.assertListEqual(puz.valid_moves(self.final_state), moves_from_final)


    def test_min_moves(self):
        self.assertEqual(puz.min_moves(self.start_state, self.final_state), 11)


if __name__ == "__main__":
    unittest.main()
