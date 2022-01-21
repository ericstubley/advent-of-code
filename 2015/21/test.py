#!/usr/bin/env python3

import unittest
from character import *
import puzzle_2015_21a as a
import puzzle_2015_21b as b


class CharacterFightTest(unittest.TestCase):
    def setUp(self):
        self.player = Character(hp=8, damage=5, armor=5)
        self.boss = Character(hp=12, damage=7, armor=2)

    def test_winner(self):
        self.assertTrue(self.player.beats(self.boss))
        self.assertTrue(self.boss.beats(self.player))


if __name__ == "__main__":
    unittest.main()
