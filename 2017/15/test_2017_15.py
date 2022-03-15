#! /usr/bin/env python3

import unittest
import puzzle_2017_15 as puz


class Test_2017_15(unittest.TestCase):
    def test_generator(self):
        gen_a = puz.Generator(seed=16807, start=65)
        self.assertEqual(next(gen_a), 1092455)
        self.assertEqual(next(gen_a), 1181022009)
        self.assertEqual(next(gen_a), 245556042)
        self.assertEqual(next(gen_a), 1744312007)
        self.assertEqual(next(gen_a), 1352636452)

    def test_picky_generator(self):
        gen_a = puz.Generator(seed=16807, start=65, multiple=4)
        self.assertEqual(next(gen_a), 1352636452)
        self.assertEqual(next(gen_a), 1992081072)
        self.assertEqual(next(gen_a), 530830436)
        self.assertEqual(next(gen_a), 1980017072)
        self.assertEqual(next(gen_a), 740335192)


    def test_match_16_bits(self):
        self.assertEqual(puz.match_16_bits(1092455, 430625591), False)
        self.assertEqual(puz.match_16_bits(1181022009, 1233683848), False)
        self.assertEqual(puz.match_16_bits(245556042, 1431495498), True)
        self.assertEqual(puz.match_16_bits(1744312007, 137874439), False)
        self.assertEqual(puz.match_16_bits(1352636452, 285222916), False)


    def test_judge(self):
        gen_a = puz.Generator(seed=16807, start=65)
        gen_b = puz.Generator(seed=48271, start=8921)
        self.assertEqual(puz.judge(40000000, gen_a, gen_b), 588)


    def test_picky_judge(self):
        gen_a = puz.Generator(seed=16807, start=65, multiple=4)
        gen_b = puz.Generator(seed=48271, start=8921, multiple=8)
        self.assertEqual(puz.judge(5000000, gen_a, gen_b), 309)


if __name__ == "__main__":
    unittest.main()
