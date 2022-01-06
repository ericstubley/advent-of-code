#!/usr/bin/env python3

from circuit import *


if __name__ == "__main__":
    with open("input.txt") as f:
        instructions = f.readlines()

    # for part b we override the input to wire b to be the answer from part a
    # that answer is 3176
    instructions[3] = "3176 -> b\n"

    builder = CircuitBuilder()
    builder.build_circuit_from_list(instructions)

    circuit = builder.circuit
    circuit.compute_outputs()

    print(circuit.output_to_int('a'))
