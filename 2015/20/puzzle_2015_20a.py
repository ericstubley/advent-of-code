#!/usr/bin/env python3

import numpy as np


def divisor_sum(n):
    ds = 0
    for i in range(1, n+1):
        if n%i == 0:
            ds += i
    return ds


if __name__ == "__main__":

    # naive upper bound from experiments of 1021020 = 2 * 2 * 3 * 5 * 7 * 11 * 13 * 17
    # 1000000 is the new upper bound

    houses = np.zeros(1000000, dtype=int)
    for elf in range(1000000):
        houses[elf::elf+1] += 10*(elf+1)

    print(min([i+1 for i in range(1000000) if houses[i] >= 36000000]))