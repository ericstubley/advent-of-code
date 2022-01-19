#!/usr/bin/env python3

import unittest
import puzzle_2015_19a as a
import puzzle_2015_19b as b


class MoleculeTest(unittest.TestCase):
    def setUp(self):
        self.rules = {'e': ['H', 'O'], 'H': ['OH', 'HO'], 'O': ['HH']}
        self.test1 = "HOH"
        self.test2 = "HOHOHO"

    def test_generate_molecules(self):
        set1 = a.generate_molecules(self.test1, self.rules)
        set2 = a.generate_molecules(self.test2, self.rules)

        set1_actual = set(["HOOH", "OHOH", "HOHO", "HHHH"])

        self.assertSetEqual(set1, set1_actual)

        self.assertEqual(len(set1), 4)
        self.assertEqual(len(set2), 7)


if __name__ == "__main__":
    unittest.main()