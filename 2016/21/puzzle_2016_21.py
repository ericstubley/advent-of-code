#! /usr/bin/env python3

from functools import partial


UNROTATE = {0:-1, 1:-1, 2:-6, 3:-2, 4:-7, 5:-3, 6:0, 7:-4}


def swap_position(ls, m, M):
    new_ls = ls.copy()
    new_ls[m], new_ls[M] = new_ls[M], new_ls[m]
    return new_ls


def swap_letter(ls, c, d):
    ret = []
    for x in ls:
        if x == c:
            ret.append(d)
        elif x == d:
            ret.append(c)
        else:
            ret.append(x)
    return ret


def rotate_absolute(ls, n):
    # right is positive
    dist = n % len(ls)
    return ls[-dist:] + ls[0:-dist]


def rotate_position(ls, c):
    i = ls.index(c)
    n = 1 + i
    if i >= 4:
        n += 1
    return rotate_absolute(ls, n)


def unrotate_position(ls, c):
    i = ls.index(c)
    return rotate_absolute(ls, UNROTATE[i]) 


def reverse(ls, m, M):
    if m == 0:
        return ls[M::-1] + ls[M+1:]
    else:
        return ls[0:m] + ls[M:m-1:-1] + ls[M+1:]


def move(ls, m, n):
    c = ls[m]
    new_ls = ls.copy()
    new_ls.remove(c)
    return new_ls[0:n] + [c] + new_ls[n:]



def parse(data):
    forwards, backwards = [], []
    for line in data:
        split = line.rstrip('\n').split(' ')
        if split[0] == "swap" and split[1] == "position":
            x, y = int(split[2]), int(split[5])
            m, M = min(x, y), max(x, y)
            forwards.append(partial(swap_position, m=m, M=M))
            backwards.append(partial(swap_position, m=m, M=M))
        elif split[0] == "swap" and split[1] == "letter":
            c, d = split[2], split[5]
            forwards.append(partial(swap_letter, c=c, d=d))
            backwards.append(partial(swap_letter, c=c, d=d))
        elif split[0] == "rotate" and split[1] == "left": 
            rl = int(split[2]) 
            forwards.append(partial(rotate_absolute, n=-1*rl))
            backwards.append(partial(rotate_absolute, n=rl))
        elif split[0] == "rotate" and split[1] == "right":
            rr = int(split[2]) 
            forwards.append(partial(rotate_absolute, n=rr))
            backwards.append(partial(rotate_absolute, n=-1*rr))
        elif split[0] == "rotate" and split[1] == "based":
            c = split[-1]
            forwards.append(partial(rotate_position, c=c))
            backwards.append(partial(unrotate_position, c=c))
        elif split[0] == "reverse":
            i, j = int(split[2]), int(split[4])
            assert i < j
            forwards.append(partial(reverse, m=i, M=j))
            backwards.append(partial(reverse, m=i, M=j))
        elif split[0] == "move":
            m, n = int(split[2]), int(split[5])
            forwards.append(partial(move, m=m, n=n))
            backwards.append(partial(move, m=n, n=m))
        else:  # something went wrong!
            assert False == True

    backwards = backwards[::-1]

    return forwards, backwards


def apply_instructions(password, instructions):
    # convert to list
    ls = list(password)

    # apply each instruction
    for f in instructions:
        ls = f(ls)

    # join up list and return
    return "".join(ls)


def main_a(password, instructions):
    print(apply_instructions(password, instructions))


def main_b(scrambled, forwards, backwards):
    unscrambled = apply_instructions(scrambled, backwards)
    assert scrambled == apply_instructions(unscrambled, forwards)
    print(unscrambled)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    forwards, backwards = parse(data)
    password = "abcdefgh"
    to_unscramble = "fbgdceah"
    main_a(password, forwards)
    main_b(to_unscramble, forwards, backwards)
