#!/usr/bin/env python3

from hashlib import md5


def get_md5_prefix(to_hash, length):
    m = md5(to_hash.encode())
    prefix = m.hexdigest()[0:length]
    return prefix


def find_lowest_for_prefix(key, prefix):
    length = len(prefix)
    number = 1

    while True:
        to_hash = key + str(number)
        hashed_prefix = get_md5_prefix(to_hash, length)

        if hashed_prefix == prefix:
            break
        number += 1

    return number


if __name__ == "__main__":
    lowest = find_lowest_for_prefix("bgvyzdsv", "000000")
    print(lowest)
