#! /usr/bin/env python3

def scan_garbage(s, i):
    assert s[i] == '<'
    count = 0
    end = i+1
    while end < len(s):
        if s[end] == '>':
            break
        elif s[end] == '!':
            end += 2
        else:
            count += 1
            end += 1
    return count, end - i + 1


def ford_stream(s):
    level, score, count, i = 0, 0, 0, 0
    while i < len(s):
        if s[i] == '{':
            level += 1
            i += 1
        elif s[i] == '}':
            score += level
            level -= 1
            i += 1
        elif s[i] == '<':
            tossed, length = scan_garbage(s, i)
            count += tossed
            i += length
        elif s[i] == ',':
            i += 1
        else:
            assert False
    return score, count


def score_stream(s):
    score, __ = ford_stream(s)
    return score


def count_garbage(s):
    __, count = ford_stream(s)
    return count


def main_a(s):
    print(score_stream(s))


def main_b(s):
    print(count_garbage(s))


if __name__ == "__main__":
    with open("input.txt") as f:
        stream = f.read()
    main_a(stream)
    main_b(stream)
