#!/usr/bin/env python3

import bitarray as ba
import bitarray.util as ba_util
from packet import *

# test packets
# D2FE28: single value packet containing 2021
# 38006F45291200: operator packet by length, containing value packets 10, 20
# EE00D40C823060: operator packet by number, containing value packets 1, 2, 3
# 8A004A801A8002F478: version sum 16 
# 620080001611562C8802118E34: version sum 12
# C0015000016115A2E0802F182340: version sum 23
# A0016C880162017C3686B18A3D4780: version sum 31 

with open("input.txt") as f:
    data = f.read().rstrip('\n')

bits = ba_util.hex2ba(data)
constructor = PacketConstructor(bits)
top_packet = constructor.build_packet()
print(top_packet.version_sum())