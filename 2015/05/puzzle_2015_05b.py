#!/usr/bin/env python3

def pair_appears_twice(string):
    ret = False
    for i in range(len(string) - 1):
        pair = string[i:i+2]
        if pair in string[i+2:]:
            ret = True
            break
    return ret


def has_xyx_pattern(string):
    ret = False
    for i in range(len(string) - 2):
        if string[i] == string[i+2]:
            ret = True
            break
    return ret


def is_nice_string(string):
    cond_1 = pair_appears_twice(string)
    cond_2 = has_xyx_pattern(string)
    return (cond_1 and cond_2)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    strings = [__.rstrip('\n') for __ in data]

    nice_count = len([__ for __ in strings if is_nice_string(__)])
    print(nice_count)
