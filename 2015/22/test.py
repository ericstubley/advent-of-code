#!/usr/bin/env python3

import unittest
from spell import *
import puzzle_2015_22a as a
import puzzle_2015_22b as b


class RPGTest(unittest.TestCase):
    def setUp(self):
        INITIAL_GAME_STATE = GameState(10, 14, 250, 8, 0, 0, 0, "player")
        self.gs = INITIAL_GAME_STATE


    def test_run_game(self):
        turn_0 = GameState(10, 14, 250, 8, 0, 0, 0, "player")
        self.assertEqual(self.gs, turn_0)

        self.gs = a.advance_turn(self.gs, Recharge())
        turn_1 = GameState(10, 14, 21, 8, 0, 0, 5, "boss")
        self.assertEqual(self.gs, turn_1)

        self.gs = a.advance_turn(self.gs, None)
        turn_2 = GameState(2, 14, 122, 8, 0, 0, 4, "player")
        self.assertEqual(self.gs, turn_2)

        self.gs = a.advance_turn(self.gs, Shield())
        turn_3 = GameState(2, 14, 110, 8, 6, 0, 3, "boss")
        self.assertEqual(self.gs, turn_3)

        self.gs = a.advance_turn(self.gs, None)
        turn_4 = GameState(1, 14, 211, 1, 5, 0, 2, "player")
        self.assertEqual(self.gs, turn_4)

        self.gs = a.advance_turn(self.gs, Drain())
        turn_5 = GameState(3, 12, 239, 1, 4, 0, 1, "boss")
        self.assertEqual(self.gs, turn_5)

        self.gs = a.advance_turn(self.gs, None)
        turn_6 = GameState(2, 12, 340, 1, 3, 0, 0, "player")
        self.assertEqual(self.gs, turn_6)

        self.gs = a.advance_turn(self.gs, Poison())
        turn_7 = GameState(2, 12, 167, 1, 2, 6, 0, "boss")
        self.assertEqual(self.gs, turn_7)

        self.gs = a.advance_turn(self.gs, None)
        turn_8 = GameState(1, 9, 167, 1, 1, 5, 0, "player")
        self.assertEqual(self.gs, turn_8)

        self.gs = a.advance_turn(self.gs, MagicMissile())
        turn_9 = GameState(1, 2, 114, 1, 0, 4, 0, "boss")
        self.assertEqual(self.gs, turn_9)



if __name__ == "__main__":
    unittest.main()