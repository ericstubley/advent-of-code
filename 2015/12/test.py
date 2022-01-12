#!/usr/bin/env python3

import unittest
from puzzle_2015_12a import number_sum
from santa_json import *

class JSONParseTest(unittest.TestCase):
    def setUp(self):
        self.array = JSONParser('[1,{"c":"red","b":2},3]').parse()
        print(self.array)
        self.object = JSONParser('{"d":"red","e":[1,2,3,4],"f":5}').parse()
        print(self.object)
        self.object_2 = JSONParser('{"a":["violet"],"b":87}').parse()
        print(self.object_2)

    def test_number_sum(self):
        self.assertEqual(number_sum("[1, 2, 3]"), 6)
        self.assertEqual(number_sum("{'a':-1,'b':4}"), 3)
        self.assertEqual(number_sum("[]"), 0)
        self.assertEqual(number_sum('{"a":{"b":4},"c":-1}'), 3)

    def test_red_sum(self):
        self.assertEqual(self.array.red_sum(), 4)
        self.assertEqual(self.object.red_sum(), 0)
        self.assertEqual(self.object_2.red_sum(), 87)


if __name__ == "__main__":
    unittest.main()
