#! /usr/bin/env python3

def checksum(bitstring):
    cs = list(bitstring)
    while len(cs) % 2 == 0:
        ls = []
        for i in range(len(cs)//2):
            segment = cs[2*i:2*i+2]
            if segment == ["0", "0"] or segment == ["1", "1"]:
                ls.append("1")
            else: # 10 or 01
                ls.append("0")
        cs = ls
    return "".join(cs)


def fill_to_length(length, initial):
    cs = list(initial)
    while len(cs) < length:
        n = len(cs)
        cs.append("0")
        for i in range(n-1, -1, -1):
            if cs[i] == "0":
                cs.append("1")
            else: 
                cs.append("0")
    return "".join(cs[:length])


def main_a(length, initial):
    print(checksum(fill_to_length(length, initial)))

def main_b(length, initial):
    print(checksum(fill_to_length(length, initial)))

if __name__ == "__main__":
    disk_length_a = 272
    disk_length_b = 35651584
    initial = "10001110011110000"
    main_a(disk_length_a, initial)
    main_b(disk_length_b, initial)
