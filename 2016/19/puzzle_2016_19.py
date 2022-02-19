#! /usr/bin/env python3

from collections import deque


class CircularLinkedListNode:
    def __init__(self, data, before=None, after=None):
        self.data = data
        self.before = before
        self.after = after

    def delete(self):
        self.before.after, self.after.before = self.after, self.before


def last_one_standing_across(n):
    # build the circular linked list
    first, mid = CircularLinkedListNode(data=(1, 1)), None
    curr, prev = None, first
    for i in range(2, n+1):
        curr = CircularLinkedListNode(data=(i, 1), before=prev)
        prev.after = curr
        if i == (n//2) + 1:
            mid = curr
        prev = curr
    # link to complete the loop
    first.before = curr
    curr.after = first

    # iterate through the circle until there's just one left
    # its just careful pointer arithmetic
    num_remaining = n
    curr = first
    while num_remaining > 1:
        curr.data = (curr.data[0], curr.data[1] + mid.data[1])
        new_mid = mid.after

        if num_remaining % 2 == 1:
            new_mid = new_mid.after
        mid.delete()
        num_remaining -= 1
        mid = new_mid

        # its important to do this step after deleting mid when 
        # the remaining number is small
        curr = curr.after

    assert curr.data[1] == n
    return curr.data[0]


def last_one_standing_next(n):
    queue = deque((i, 1) for i in range(1, n+1))

    while len(queue) > 1:
        elf, presents = queue.popleft()
        assert presents > 0
        __, stolen = queue.popleft()
        queue.append((elf, presents + stolen))

    elf, presents = queue.popleft()
    assert presents == n
    return elf 


def main_a(num_elves):
    print(last_one_standing_next(num_elves))


def main_b(num_elves):
    print(last_one_standing_across(num_elves))


if __name__ == "__main__":
    num_elves = 3005290
    main_a(num_elves)
    main_b(num_elves)
