#! /usr/bin/env python3

import unittest
import puzzle_2016_21 as puz


class Test_2016_21(unittest.TestCase):
    def setUp(self):
        self.password = "abcde"
        with open("test_input.txt") as f:
            data = f.readlines()
        self.forwards, self.backwards = puz.parse(data)

    def test_scramble(self):
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:1]), "ebcda") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:2]), "edcba") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:3]), "abcde") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:4]), "bcdea") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:5]), "bdeac") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:6]), "abdec") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:7]), "ecabd") 
        self.assertEqual(puz.apply_instructions(self.password, self.forwards[0:8]), "decab") 

    def test_unscramble(self):
        # this works even though the length is different????
        # I thought the length being 8 figured into the unrotate calculations
        # maybe the input was clever
        self.assertEqual(puz.apply_instructions("decab", self.backwards), "abcde")


if __name__ == "__main__":
    unittest.main()
