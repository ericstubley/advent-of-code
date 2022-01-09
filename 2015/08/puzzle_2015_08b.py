#!/usr/bin/env python3

def count_code_characters(string):
    return len(string)


def encode_string(string):
    ret = ['"']

    for c in string:
        if c == '\\':
            ret.append('\\\\')
        elif c == '"':
            ret.append('\\"')
        else:
            ret.append(c)

    ret.append('"')
    return ''.join(ret)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    strings = [__.rstrip('\n') for __ in data]
    total_diff = 0
    for string in strings:
        total_diff += count_code_characters(encode_string(string))
        total_diff -= count_code_characters(string)

    print(total_diff)
