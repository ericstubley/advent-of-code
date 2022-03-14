#! /usr/bin/env python3

from functools import reduce


def tie(string, position, length):
    if position + length > len(string):
        overflow = position + length - len(string)
        to_reverse = string[position:] + \
                string[0: overflow]
        assert len(to_reverse) == length
        reverse = list(reversed(to_reverse))
        ret = reverse[-overflow:] + \
                string[overflow:position] + \
                reverse[0:-overflow]
        assert len(ret) == len(string)
        return ret
    else:
        ret = string[0:position] + \
                list(reversed(string[position:position+length])) + \
                string[position+length:]
        assert len(ret) == len(string)
        return ret


def hash(string, lengths, position, skip):
    for l in lengths:
        string = tie(string, position, l)
        position = (position + l + skip) % len(string)
        skip += 1
    return string, position, skip


def sparse_hash(string, lengths):
    position, skip = 0, 0
    for i in range(64):
        string, position, skip = hash(string, lengths, position, skip)
    return string


def s_to_lengths(s):
    return [ord(c) for c in s]


def sparse_to_dense(string):
    ret = []
    for i in range(16):
        chunk = reduce(lambda x, y: x^y, string[16*i:16*(i+1)])
        ret.append(hex(chunk)[2:].rjust(2, '0'))
    return "".join(ret)


def dense_hash(s, salt):
    lengths = s_to_lengths(s) + salt
    string = [i for i in range(256)]
    sparse = sparse_hash([i for i  in range(256)], lengths)
    return sparse_to_dense(sparse)


def main_a(lengths):
    hashed, __, __ = hash([i for i in range(256)], lengths, 0, 0)
    print(hashed[0]*hashed[1])


def main_b(s):
    print(dense_hash(s, salt=[17, 31, 73, 47, 23]))


if __name__ == "__main__":
    lengths = [197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63]
    s = ",".join(str(i) for i in lengths)
    main_a(lengths)
    main_b(s)
