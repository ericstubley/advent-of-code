#! /usr/bin/env python3

def better_program(a, b, c, d):

    # Lines 1 - 2
    a, b, c, d = a, a-1, c, d

    # While waiting for the 24, 22, 20, 18 toggles to happen
    while b > 1:
        # Lines 3 - 10
        a, b, c, d = a*b, b, 0, 0
        # Lines 11 - 16
        a, b, c, d = a, b-1, 2*(b-1), 0

    # Lines 20 - 26
    a, b, c, d = a + 80*93, b, 0, 0

    return a, b, c, d


if __name__ == "__main__":
    print(better_program(7, 0, 0, 0))
    print(better_program(12, 0, 0, 0))
