#!/usr/bin/env python3

C0 = 20151125
MOD = 33554393
MULT = 252533

def code_by_index(n):
    return (C0 * pow(MULT, n-1, MOD)) % MOD


def code_by_grid(row, col):
    diag_end = row + col - 1
    triangle = (diag_end * (diag_end + 1)) // 2
    n = triangle - row + 1

    return code_by_index(n)


if __name__ == "__main__":
    print(code_by_grid(2947, 3029))