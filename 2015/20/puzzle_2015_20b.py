#!/usr/bin/env python3

from collections import defaultdict

if __name__ == "__main__":
    houses = defaultdict(int)
    elf = 1
    for i in range(1, 51):
        houses[elf*i] += 11*elf

    while houses[elf] < 36000000:
        del houses[elf] 

        elf += 1
        print(f"\r{elf}", end="")
        for i in range(1, 51): 
            houses[elf*i] += 11*elf
     
    print(" ")
    print(elf, houses[elf])