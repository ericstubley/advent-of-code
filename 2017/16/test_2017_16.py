#! /usr/bin/env python3

import unittest
import puzzle_2017_16 as puz


class Test_2017_16(unittest.TestCase):
    def setUp(self):
        self.index_steps, self.char_steps = puz.parse_steps("test_input.txt")

    def test_instructions(self):
        self.assertEqual(self.index_steps[0]("abcde"), "eabcd")
        self.assertEqual(self.index_steps[1]("eabcd"), "eabdc")
        self.assertEqual(self.char_steps[0]("eabdc"), "baedc")

    def test_dance(self):
        danced = puz.dance(puz.dance("abcde", self.index_steps), self.char_steps)
        self.assertEqual(danced, "baedc")

    def test_extract_permutation(self):
        extracted = puz.extract_permutation("abcd", "bdca")
        computed = {0:3, 1:0, 2:2, 3:1}
        self.assertEqual(extracted, computed)

    def test_compose_permutation(self):
        sigma = {0:5, 1:1, 2:3, 3:4, 4:2, 5:0}
        tau = {0:4, 1:1, 2:2, 3:0, 4:3, 5:5}

        computed = {0:2, 1:1, 2:3, 3:5, 4:4, 5:0}

        self.assertEqual(puz.compose_permutation(sigma, tau), computed)


    def test_power_permutation(self):
        sigma = {0:5, 1:1, 2:3, 3:4, 4:2, 5:0}
        tau = {0:4, 1:1, 2:2, 3:0, 4:3, 5:5}
        st = puz.compose_permutation(sigma, tau)
        ident = puz.identity_permutation(6)
        self.assertEqual(puz.power_permutation(sigma, 720), ident)
        self.assertEqual(puz.power_permutation(tau, 720), ident)
        self.assertEqual(puz.power_permutation(st, 720), ident)


    def test_apply_permutation(self):
        sigma = {0:3, 1:0, 2:2, 3:1}
        self.assertEqual(puz.apply_index_permutation(sigma, "abcd"), "bdca")


    def test_repeat_dance(self):
        self.assertEqual(puz.repeat_dance("abcde", self.index_steps, self.char_steps, 1), "baedc")
        self.assertEqual(puz.repeat_dance("abcde", self.index_steps, self.char_steps, 2), "ceadb")


if __name__ == "__main__":
    unittest.main()
