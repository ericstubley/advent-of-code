#!/usr/bin/env python3


VOWELS = ['a', 'e', 'i', 'o', 'u']
BANNED_SUBSTRINGS = ['ab', 'cd', 'pq', 'xy']


def has_three_vowels(string):
    vowel_count = 0
    for v in VOWELS:
        vowel_count += string.count(v)
    return vowel_count >= 3


def has_double_letter(string):
    ret = False
    for i in range(len(string) - 1):
        if string[i] == string[i+1]:
            ret = True
            break
    return ret


def has_banned_substring(string):
    ret = False
    for s in BANNED_SUBSTRINGS:
        if s in string:
            ret = True
            break
    return ret


def is_nice_string(string):
    cond_1 = has_three_vowels(string)
    cond_2 = has_double_letter(string)
    cond_3 = not has_banned_substring(string)
    return (cond_1 and cond_2 and cond_3)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    strings = [__.rstrip('\n') for __ in data]

    nice_count = len([__ for __ in strings if is_nice_string(__)])
    print(nice_count)
