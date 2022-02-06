#! /usr/bin/env python3

from hashlib import md5

POSITIONS = ["0", "1", "2", "3", "4", "5", "6", "7"]


def next_digit_a(door_id, index):
    while True:
        m = md5() 
        s = door_id + str(index)
        m.update(s.encode())
        d = m.hexdigest()
        if d.startswith("00000"):
            return index, d[5]
        index += 1


def crack_password_a(door_id):
    digits = []
    index = 0
    while len(digits) < 8:
        index, digit = next_digit_a(door_id, index)
        digits.append(digit)
        print(len(digits))
        index += 1
    return "".join(digits) 


def next_digit_b(door_id, index):
    while True:
        m = md5()
        s = door_id + str(index)
        m.update(s.encode())
        d = m.hexdigest()
        if d.startswith("00000") and d[5] in POSITIONS:
            return index, int(d[5]), d[6]
        index += 1


def crack_password_b(door_id):
    digits = [None]*8
    found, index = 0, 0
    while found < 8:
        index, loc, digit = next_digit_b(door_id, index)
        if digits[loc] is None:
            digits[loc] = digit
            found += 1
            print(found)
        index += 1
    return "".join(digits)



def main_a(door_id):
    pass
    # print(crack_password_a(door_id))

def main_b(door_id):
    print(crack_password_b(door_id))

if __name__ == "__main__":
    door_id = "ffykfhsq" 
    main_a(door_id)
    main_b(door_id)
