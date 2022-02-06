#! /usr/bin/env python3

from collections import defaultdict


LETTERS = [chr(ord('a') + i) for i in range(26)]


def parse(data):
    return [__.rstrip('\n') for __ in data]


def extract_message(messages, extractor):
    counts = defaultdict(int)
    for m in messages:
        for i, l in enumerate(m):
            counts[(i, l)] += 1
    ret = []
    for i in range(len(messages[0])):

        index = extractor([counts[(i, l)] for l in LETTERS])
        ret.append(chr(ord('a') + index))

    return "".join(ret)


def max_extractor(ls):
    # given a list of counts for each letter, return index of max
    max_count = max(ls)
    index = ls.index(max_count)
    return index


def min_extractor(ls):
    # return index of letter which has min non-zero count
    min_index, min_count = 0, max(ls)
    for i, x in enumerate(ls):
        if x > 0 and x < min_count:
            min_index = i
            min_count = x
    return min_index


def main_a(messages):
    print(extract_message(messages, max_extractor))


def main_b(messages):
    print(extract_message(messages, min_extractor))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    messages = parse(data)

    main_a(messages)
    main_b(messages)
