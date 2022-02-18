#! /usr/bin/env python3

import bitarray as ba
# basic model of this version is that each row is a bitarray
# 1 represents a trap, 0 represents safe


def count_safe_tiles(first_row, num_rows):
    count = first_row.count(0)
    curr_row = first_row
    for i in range(1, num_rows):
        curr_row = (curr_row >> 1) ^ (curr_row << 1)
        count += curr_row.count(0)
    return count


def main_a(first_row):
    print(count_safe_tiles(first_row, 40))


def main_b(first_row):
    print(count_safe_tiles(first_row, 400000))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.read()

    first_row = data.rstrip('\n')
    first_row = ba.bitarray(first_row.replace('^', '1').replace('.', '0'))
    main_a(first_row)
    main_b(first_row)
