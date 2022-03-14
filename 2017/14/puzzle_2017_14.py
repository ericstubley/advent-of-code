#! /usr/bin/env python3

from collections import deque

from automation.automation import submit_answer
from puzzle_2017_10 import dense_hash


HEX_TO_NBITS = {'0': 0,'1': 1, '2': 1, '3': 2,
    '4': 1, '5': 2, '6': 2, '7': 3,
    '8': 1, '9': 2, 'a': 2, 'b': 3,
    'c': 2, 'd': 3, 'e': 3, 'f': 4}

HEX_TO_DIGITS = {'0': [0, 0, 0, 0],
    '1': [0, 0, 0, 1],
    '2': [0, 0, 1, 0],
    '3': [0, 0, 1, 1],
    '4': [0, 1, 0, 0],
    '5': [0, 1, 0, 1],
    '6': [0, 1, 1, 0],
    '7': [0, 1, 1, 1],
    '8': [1, 0, 0, 0],
    '9': [1, 0, 0, 1],
    'a': [1, 0, 1, 0],
    'b': [1, 0, 1, 1],
    'c': [1, 1, 0, 0],
    'd': [1, 1, 0, 1],
    'e': [1, 1, 1, 0],
    'f': [1, 1, 1, 1]}


def row_digits(key_string, row):
    row_string = key_string + "-" + str(row)
    return dense_hash(row_string, salt=[17, 31, 73, 47, 23])


def count_row(digits):
    count = 0
    for d in digits:
        count += HEX_TO_NBITS[d]
    return count


def disk_usage(key_string):
    used_bits = 0
    for r in range(128):
        digits = row_digits(key_string, r)
        used_bits += count_row(digits)
    return used_bits


def print_grid(grid):
    for row in grid:
        row_ls = ['#' if x == 1 else '.' for x in row]
        print("".join(row_ls))


def build_grid(key_string):
    grid, locations = [], []
    for i in range(128):
        row = []
        digits = row_digits(key_string, i)
        for j in range(32):
            local = HEX_TO_DIGITS[digits[j]]
            row += local
            for k, c in enumerate(local):
                if c == 1:
                    locations.append((i, 4*j + k))
        grid.append(row)
    return grid, locations


def neighbours(x, y):
    ret = []
    if x > 0:
        ret.append((x-1, y))
    if x < 127:
        ret.append((x+1, y))
    if y > 0:
        ret.append((x, y-1))
    if y < 127:
        ret.append((x, y+1))
    return ret


def count_regions(key_string):
    grid, locations = build_grid(key_string)
    seen = set()
    region_count = 0
    for x, y in locations:
        if grid[x][y] == 1 and (x, y) not in seen:
            region_count += 1
            seen.add((x, y))
            queue = deque([(x, y)])
            while len(queue) > 0:
                r, s = queue.popleft()
                for u, v in neighbours(r, s):
                    if grid[u][v] == 1 and (u, v) not in seen:
                        queue.append((u, v))
                        seen.add((u, v))
    return region_count


def main_a(key_string):
    answer = disk_usage(key_string)
    print(answer)
    # result = submit_answer(2017, 14, 1, answer)
    # print(result)


def main_b(key_string):
    answer = count_regions(key_string)
    print(answer)
    result = submit_answer(2017, 14, 2, answer)
    print(result)


if __name__ == "__main__":
    key_string = "amgozmfv" 
    main_a(key_string)
    main_b(key_string)
