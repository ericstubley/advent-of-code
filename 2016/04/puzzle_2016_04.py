#! /usr/bin/env python3

from collections import namedtuple


EncryptedRoom = namedtuple("EncryptedRoom", ["name", "sector", "checksum"])
LETTERS = [chr(ord('a')+i) for i in range(26)]


def parse(data):
    ret = []
    for line in data:
        line = line.rstrip('\n')

        # last characters are -XYZ[abcde]
        # name is everything through -11
        # sector is -10 through -7
        # checksum is -6 through -1
        name  = line[:-11]
        sector = int(line[-10:-7])
        checksum = line[-6:-1]

        ret.append(EncryptedRoom(name=name, sector=sector, checksum=checksum))
    return ret


def chr_to_num(c):
    return ord(c) - ord('a')

def num_to_chr(n):
    return chr(ord('a') + n)


def letter_counts(name):
    lc = [0]*26
    for c in name:
        num = chr_to_num(c)
        if 0 <= num and num < 26:
            lc[num] += 1
    return lc


def is_valid(room):
    lc = letter_counts(room.name)
    freqs = sorted(zip(lc, LETTERS), key=lambda x: (-1*x[0],x[1]))
    checksum = "".join(__[1] for __ in freqs[0:5])

    return checksum == room.checksum


def decrypt_name(room):
    decrypt_ls = []
    shift = room.sector % 26
    for c in room.name:
        if c == '-':
            decrypt_ls.append(' ')
        else:
            shifted = num_to_chr((chr_to_num(c) + shift) % 26)
            decrypt_ls.append(shifted)
    return "".join(decrypt_ls)


def valid_sector_id_sum(rooms):
    return sum(r.sector for r in rooms if is_valid(r))


def main_a(rooms):
    print(valid_sector_id_sum(rooms))


def main_b(rooms):
    for r in rooms:
        if is_valid(r):
            actual_name = decrypt_name(r)
            if "north" in actual_name or "pole" in actual_name or "object" in actual_name or "stor" in actual_name:
                print(actual_name, r.name, r.sector)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    rooms = parse(data)

    main_a(rooms)
    main_b(rooms)
