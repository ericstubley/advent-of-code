#! /usr/bin/env python3

import numpy as np
from collections import Counter
from automation.automation import submit_answer


INITIAL_CANVAS = ".#./..#/###"


def parse_rules(filename):
    with open(filename) as f:
        data = f.readlines()

    rules = dict()
    for line in data:
        inp, __, out = line.split()
        m_in, m_out = str_to_array(inp), str_to_array(out)
        for transformed in transforms(m_in):
            rules[transformed.tobytes()] = m_out
    return rules


def array_to_str(m):
    lines = []
    for row in m:
        line = ['#' if x == 1 else '.' for x in row]
        lines.append("".join(line))
    return '/'.join(lines)


def str_to_array(s):
    array = []
    for line in s.split('/'):
        array.append([1 if x == '#' else 0 for x in line])
    return np.array(array, dtype=bool)


def evolve(s, rules, t, output=False):
    m = str_to_array(s)
    for time in range(t):
        m = evolve_array(m, rules)
        if output:
            print(time)
    return array_to_str(m)


def chunkify(m):
    rows, cols = m.shape
    num_chunks = (rows // 2) if rows % 2 == 0 else (rows // 3)
    # the vsplit and hsplit look flipped but these are right
    return [np.hsplit(cs, num_chunks) for cs in np.vsplit(m, num_chunks)] 


def evolve_array(m, rules):
    # split it up
    chunks = chunkify(m)
    # evolve each chunk
    new_chunks = []
    for block_row in chunks:
        new_row = [evolve_chunk(c, rules) for c in block_row]
        new_chunks.append(new_row)
    # block them back up and return
    return np.block(new_chunks)


def evolve_chunk(m, rules):
    # find the match and return it
    # rules now has all the transforms preloaded
    key = m.tobytes()
    return rules[key]


def transforms(m):
    for k in range(4):
        yield np.rot90(m, k=k)
    for k in range(4):
        yield np.rot90(np.fliplr(m), k=k)


def count_on(grid):
    if isinstance(grid, str):
        return grid.count('#')
    else:
        return np.count_nonzero(grid)


def independent_evolve(s, rules, t=18, output=False):
    # s should be a 3x3 start, and t should be divisible by 3
    m = str_to_array(s)

    chunk_counts = Counter()
    chunk_counts[m.tobytes()] = 1
    evolutions = dict()

    for time in range(t // 3):
        if output:
            print(time)

        new_counts = Counter() 
        for c in chunk_counts:
            if c not in evolutions:
                evolutions[c] = independent_chunk_evolve(c, rules)
            for d in evolutions[c]:
                new_counts[d] += chunk_counts[c]

        chunk_counts = new_counts

    return sum(chunk_counts[c] * count_on(np.frombuffer(c, dtype=bool)) for c in chunk_counts)


def independent_chunk_evolve(c, rules):
        cm = np.reshape(np.frombuffer(c, dtype=bool), (3, 3))
        for i in range(3):
            cm = evolve_array(cm, rules)

        chunks = chunkify(cm)
        ls = []
        for row in chunks:
            for chunk in row:
                ls.append(chunk.tobytes())
        return ls 


def main_a(rules):
    answer = count_on(evolve(INITIAL_CANVAS, rules, t=5))
    print(answer)
    # result = submit_answer(2017, 21, 1, answer)
    # print(result)


def main_b(rules):
    answer = independent_evolve(INITIAL_CANVAS, rules, t=18, output=True)
    print(answer)
    # result = submit_answer(2017, 21, 2, answer)
    # print(result)


if __name__ == "__main__":
    rules = parse_rules("input.txt")
    main_a(rules)
    main_b(rules)
