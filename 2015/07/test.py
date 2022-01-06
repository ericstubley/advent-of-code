#!/usr/bin/env python3

import unittest
from circuit import *


class CircuitTest(unittest.TestCase):
    def setUp(self):
        self.builder = CircuitBuilder()

        with open("test_input.txt") as f:
            self.instructions = f.readlines()

        self.builder.build_circuit_from_list(self.instructions)
        self.circuit = self.builder.circuit

        self.circuit.compute_outputs()

    def test_the_simple_circuit(self):
        """ d: 72
            e: 507
            f: 492
            g: 114
            h: 65412
            i: 65079
            x: 123
            y: 456"""
        self.assertEqual(self.circuit.output_to_int('d'), 72)
        self.assertEqual(self.circuit.output_to_int('e'), 507)
        self.assertEqual(self.circuit.output_to_int('f'), 492)
        self.assertEqual(self.circuit.output_to_int('g'), 114)
        self.assertEqual(self.circuit.output_to_int('h'), 65412)
        self.assertEqual(self.circuit.output_to_int('i'), 65079)
        self.assertEqual(self.circuit.output_to_int('x'), 123)
        self.assertEqual(self.circuit.output_to_int('y'), 456)


if __name__ == "__main__":
    unittest.main()
