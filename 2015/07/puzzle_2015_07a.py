#!/usr/bin/env python3

from circuit import *


if __name__ == "__main__":
    with open("input.txt") as f:
        instructions = f.readlines()

    builder = CircuitBuilder()
    builder.build_circuit_from_list(instructions)

    circuit = builder.circuit
    circuit.compute_outputs()

    print(circuit.output_to_int('a'))
