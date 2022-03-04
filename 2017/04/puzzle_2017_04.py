#! /usr/bin/env python3

def is_valid_passphrase(p):
    ret, words = True, set()
    for word in p:
        if word in words:
            ret = False
            break
        else:
            words.add(word)
    return ret


def word_to_letter_count(word):
    counts = [0]*26
    for c in word:
        counts[c_to_index(c)] += 1
    return tuple(counts)


def c_to_index(c):
    return ord(c) - ord('a')


def main_a(passphrases):
    count = 0
    for p in passphrases:
        if is_valid_passphrase(p):
            count += 1
    print(count)


def main_b(passphrases):
    count = 0
    for p in passphrases:
        ap = map(word_to_letter_count, p)
        if is_valid_passphrase(ap):
            count += 1
    print(count)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    passphrases = [line.split() for line in data]
    main_a(passphrases)
    main_b(passphrases)
