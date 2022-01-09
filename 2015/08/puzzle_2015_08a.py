#!/usr/bin/env python3

def count_code_characters(string):
    return len(string)


def count_memory_characters(string):
    count = 0
    i = 1
    while i < len(string) - 1:
        c = string[i]
        if c == '\\':
            d = string[i+1]
            if d == '"' or d == '\\':
                i += 1
            elif d == 'x':
                i += 3

        count += 1
        i += 1
    return count


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    strings = [__.rstrip('\n') for __ in data]
    total_diff = 0
    for string in strings:
        total_diff += count_code_characters(string)
        total_diff -= count_memory_characters(string)

    print(total_diff)