#! /usr/bin/env python3

from hashlib import md5


class OneTimePad:
    def __init__(self, salt, stretch=False):
        self.salt = salt
        self.stretch = stretch
        self.hashes = []


    def get_hash(self, index):
        if len(self.hashes) > index:
            return self.hashes[index]

        if len(self.hashes) < index:
            self.get_hash(index-1)

        assert len(self.hashes) == index

        h = self.compute_hash(index)
        self.hashes.append(h)
        return h


    def compute_hash(self, index):
        m = md5()
        s = self.salt + str(index) 
        m.update(s.encode())
        h = m.hexdigest()

        if self.stretch:
            for i in range(2016):
                m = md5()
                m.update(h.encode())
                h = m.hexdigest()

        return h


    def has_triplet(self, index):
        h = self.get_hash(index)
        c = None
        for i in range(len(h)-2):
            if h[i]*3 == h[i:i+3]:
                c = h[i]
                break

        return False if c is None else c


    def has_quintuplet(self, index, c):
        h = self.get_hash(index)
        ret = False
        for i in range(len(h)-4):
            if c*5 == h[i:i+5]:
                ret = True
                break
        return ret


    def is_key(self, index):
        c = self.has_triplet(index)
        if c is False:
            return False
        else:
            ret = False
            for i in range(1, 1001):
                if self.has_quintuplet(index+i, c):
                    ret = True
                    break
            return ret


    def next_key(self, index):
        i = index
        while True:
            if self.is_key(i):
                return i
            else:
                i += 1


    def nth_key(self, n):
        key = -1
        for i in range(n):
            key = self.next_key(key+1)
            print(f"Key {i+1} is {key}")
        return key


def main_a(otp):
    print(otp.nth_key(64))


def main_b(otp):
    print(otp.nth_key(64))


if __name__ == "__main__":
    salt = "zpqevtbw"
    otp = OneTimePad(salt)
    otp_stretch = OneTimePad(salt, stretch=True)

    main_a(otp)
    main_b(otp_stretch)
