#! /usr/bin/env python3

import unittest
import puzzle_2016_05 as puz


class Test_2016_05(unittest.TestCase):
    def setUp(self):
        self.door_id = "abc"
        self.password_a = "18f47a30"
        self.password_b = "05ace8e3"

    def test_next_digit_a(self):
        self.assertEqual(puz.next_digit_a(self.door_id, 0), (3231929, "1"))

    def test_crack_password_a(self):
        self.assertEqual(puz.crack_password_a(self.door_id), self.password_a)

    def test_next_digit_b(self):
        self.assertEqual(puz.next_digit_b(self.door_id, 0), (3231929, 1, "5"))
        self.assertEqual(puz.next_digit_b(self.door_id, 3231930), (5357525, 4, "e"))

    def test_crack_password_b(self):
        self.assertEqual(puz.crack_password_b(self.door_id), self.password_b)


if __name__ == "__main__":
    unittest.main()
