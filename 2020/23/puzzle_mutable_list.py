#! /usr/bin/env python3

def digits(n):
    ret = []
    while n > 0:
        ret.append(n % 10)
        n //= 10
    return list(reversed(ret))


def layout(n):
    ds = [(i-1) for i in digits(n)]
    ds.append(9)
    ls = [0]*1000000
    for i in range(9):
        ls[ds[i]] = ds[i+1]
    for i in range(9, 1000000):
        ls[i] = i+1
    ls[999999] = 0
    return ls


def shuffle(ls, curr):
    # find all the pointers
    fst = ls[curr]
    snd = ls[fst]
    trd = ls[snd]
    after_trd = ls[trd]
    dest = destination(curr, [fst, snd, trd])
    after_dest = ls[dest]

    # rewire all the pointers
    ls[curr] = after_trd
    ls[dest] = fst
    ls[trd] = after_dest
    return (ls, after_trd)


def destination(curr, banned):
    option = recenter(curr-1)
    if option not in banned:
        return option
    else:
        return destination(option, banned)


def recenter(n):
    return n % 1000000


def nbhd_of_one(ls):
    fst = ls[0]
    snd = ls[fst]
    return (fst+1)*(snd+1)


def simulate(n):
    ls = layout(n)
    curr = 0
    for i in range(10000000):
        (ls, curr) = shuffle(ls, curr)
    return ls


if __name__ == "__main__":
    n = 137826495
    ls = layout(n)
    print(ls[0:10])

    print(nbhd_of_one(simulate(n)))