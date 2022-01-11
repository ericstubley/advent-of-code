#!/usr/bin/env python3

def is_valid_password(pwd):
    cond1 = has_run(pwd)
    cond2 = no_banned_letters(pwd)
    cond3 = has_doubles(pwd)
    return (cond1 and cond2 and cond3)


def has_run(pwd):
    ret = False
    for i in range(len(pwd) - 2):
        if ord(pwd[i+1]) == ord(pwd[i]) + 1 and ord(pwd[i+2]) == ord(pwd[i]) + 2:
            ret = True
            break
    return ret        


def no_banned_letters(pwd):
    return (('i' not in pwd) and ('o' not in pwd) and ('l' not in pwd))


def has_doubles(pwd):
    doubles = set()
    for i in range(len(pwd) - 1):
        if pwd[i+1] == pwd[i]:
            doubles.add(pwd[i])
    return len(doubles) >= 2


def next_valid(pwd):
    possible = next_string(pwd)
    while not is_valid_password(possible):
        possible = next_string(possible)
    return possible


def next_string(pwd):
    if pwd[-1] != 'z':
        head = pwd[:-1]
        tail = chr(ord(pwd[-1])+1)
    else:
        head = next_string(pwd[:-1])
        tail = 'a'
    return head + tail


if __name__ == "__main__":
    print(next_valid("vzbxxyzz"))
