#! /usr/bin/env python3

import unittest
import puzzle_2016_10 as puz


class Test_2016_10(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.bots, self.outputs, self.inputs = puz.parse(data)

    def test_parse(self):
        self.assertEqual(len(self.bots), 3)
        self.assertEqual(len(self.outputs), 3)
        self.assertEqual(len(self.inputs), 3)


    def test_run_actions(self):
        puz.take_inputs(self.inputs, self.bots, self.outputs)
        self.assertEqual(self.outputs[0].value, 5)
        self.assertEqual(self.outputs[1].value, 2)
        self.assertEqual(self.outputs[2].value, 3)



if __name__ == "__main__":
    unittest.main()
