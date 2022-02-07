#! /usr/bin/env python3

import unittest
import puzzle_2016_07 as puz


class Test_2016_07(unittest.TestCase):
    def setUp(self):
        self.ip_1 = "abba[mnop]qrst"
        self.ip_2 = "abcd[bddb]xyyx"
        self.ip_3 = "aaaa[qwer]tyui"
        self.ip_4 = "ioxxoj[asdfgh]zxcvbn"

        self.ip_5 = "aba[bab]xyz"
        self.ip_6 = "xyx[xyx]xyx"
        self.ip_7 = "aaa[kek]eke"
        self.ip_8 = "zazbz[bzb]cdb"

    def test_tls_support(self):
        self.assertTrue(puz.tls_support(self.ip_1))
        self.assertFalse(puz.tls_support(self.ip_2))
        self.assertFalse(puz.tls_support(self.ip_3))
        self.assertTrue(puz.tls_support(self.ip_4))

    def test_ssl_support(self):
        self.assertTrue(puz.ssl_support(self.ip_5))
        self.assertFalse(puz.ssl_support(self.ip_6))
        self.assertTrue(puz.ssl_support(self.ip_7))
        self.assertTrue(puz.ssl_support(self.ip_8))


    def test_invert_pattern(self):
        self.assertEqual(puz.invert_pattern("aba"), "bab")


if __name__ == "__main__":
    unittest.main()
