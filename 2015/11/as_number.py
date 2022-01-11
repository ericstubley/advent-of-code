#!/usr/bin/env python3

def is_valid_password(pwd):
    ls = n_to_ls(pwd)
    cond1 = has_run(ls)
    cond2 = no_banned_letters(ls)
    cond3 = has_doubles(ls)
    return (cond1 and cond2 and cond3)


def has_run(ls):
    ret = False
    for i in range(len(ls) - 2):
        if ls[i+1] == ls[i] + 1 and ls[i+2] == ls[i] + 2:
            ret = True
            break
    return ret        


def no_banned_letters(ls):
    return ((8 not in ls) and (11 not in ls) and (14 not in ls))


def has_doubles(ls):
    doubles = set()
    for i in range(len(ls) - 1):
        if ls[i+1] == ls[i]:
            doubles.add(ls[i])
    return len(doubles) >= 2


def next_valid(pwd):
    n = s_to_n(pwd)
    n += 1

    while not is_valid_password(n):
        n += 1
    return n_to_s(n)


def ls_to_n(ls):
    return sum([d*26**i for i, d in enumerate(ls[::-1])])


def ls_to_s(ls):
    return [chr(d + 97) for d in ls]


def n_to_ls(n, length=8):
    ls = []
    head = n
    while head != 0:
        head, digit = divmod(head, 26)
        ls.append(digit)
    if len(ls) < length:
        ls.extend([0]*(length - len(ls)))
    return ls[::-1]


def n_to_s(n, length=8):
    ls = []
    head = n
    while head != 0:
        head, digit = divmod(head, 26)
        ls.append(chr(digit + 97))
    if len(ls) < length:
        ls.extend(['a']*(length - len(ls)))
    return ''.join(ls[::-1])


def s_to_ls(s):
    return [ord(c)-97 for c in s]


def s_to_n(s):
    return sum([(ord(c)-97)*26**i for i, c in enumerate(s[::-1])])




if __name__ == "__main__":
    print(next_valid("vzbxxyzz"))
