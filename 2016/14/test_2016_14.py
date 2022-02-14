#! /usr/bin/env python3

import unittest
import puzzle_2016_14 as puz


class Test_2016_14(unittest.TestCase):
    def setUp(self):
        self.salt = "abc"
        self.otp = puz.OneTimePad(self.salt)
        self.otp_stretch = puz.OneTimePad(self.salt, stretch=True)

    def test_stretch_hash(self):
        self.assertEqual(self.otp_stretch.get_hash(0), "a107ff634856bb300138cac6568c0f24")

    def test_next_key(self):
        self.assertEqual(self.otp.next_key(0), 39)
        self.assertEqual(self.otp.next_key(40), 92)
        self.assertEqual(self.otp_stretch.next_key(0), 10)

    def test_nth_key(self):
        self.assertEqual(self.otp.nth_key(64), 22728)
        self.assertEqual(self.otp_stretch.nth_key(64), 22551)

    def test_has_triplet(self):
        self.assertEqual(self.otp.has_triplet(17), False)
        self.assertEqual(self.otp.has_triplet(18), '8')
        self.assertEqual(self.otp_stretch.has_triplet(4), False)
        self.assertEqual(self.otp_stretch.has_triplet(5), '2')



if __name__ == "__main__":
    unittest.main()
