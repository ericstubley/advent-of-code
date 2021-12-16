import bitarray as ba
import bitarray.util as ba_util
import math

class Packet:
    def __init__(self, version, type_id):
        self.version = version
        self.typed_id = type_id

    def version_sum(self):
        return self.version

    def evaluate(self):
        pass

class ValuePacket(Packet):
    def __init__(self, version, type_id, value):
        self.version = version
        self.type_id = type_id
        self.value = value

    def is_value_packet(self):
        return True

    def is_operator_packet(self):
        return False

    def evaluate(self):
        return self.value

class OperatorPacket(Packet):
    def __init__(self, version, type_id, subpackets):
        self.version = version
        self.type_id = type_id
        self.subpackets = subpackets

    def is_value_packet(self):
        return False

    def is_operator_packet(self):
        return True

    def __iter__(self):
        return iter(self.subpackets)

    def version_sum(self):
        return self.version + sum([p.version_sum() for p in self])

    def evaluate(self):
        # wanted to do a match statement but apparently I'm using 3.9 and 
        # that only showed up in 3.10
        if self.type_id == 0:
            return sum([p.evaluate() for p in self.subpackets])
        elif self.type_id == 1:
            return math.prod([p.evaluate() for p in self.subpackets])
        elif self.type_id == 2:
            return min([p.evaluate() for p in self.subpackets])
        elif self.type_id == 3:
            return max([p.evaluate() for p in self.subpackets])
        elif self.type_id == 5:
            first = self.subpackets[0].evaluate()
            second = self.subpackets[1].evaluate()
            return 1 if first > second else 0
        elif self.type_id == 6:
            first = self.subpackets[0].evaluate()
            second = self.subpackets[1].evaluate()
            return 1 if first < second else 0
        elif self.type_id == 7:
            first = self.subpackets[0].evaluate()
            second = self.subpackets[1].evaluate()
            return 1 if first == second else 0


class PacketConstructor:
    def __init__(self, bits):
        self.bits = bits

    def build_packet(self):
        if not self.bits.any():
            return None

        type_id = ba_util.ba2int(self.bits[3:6])
        if type_id == 4:
            return self.build_value_packet()
        else:
            return self.build_operator_packet()

    def build_value_packet(self):
        version = ba_util.ba2int(self.bits[0:3])
        type_id = ba_util.ba2int(self.bits[3:6])
        self.bits = self.bits[6:]

        value_ba = ba.bitarray() 
        while True:
            last_flag = True if self.bits[0] == 0 else False
            value_ba.extend(self.bits[1:5])
            self.bits = self.bits[5:]
            if last_flag:
                break

        value = ba_util.ba2int(value_ba)
        return ValuePacket(version, type_id, value)

    def build_operator_packet(self):
        length_type_id = self.bits[6]
        if length_type_id == 0:
            return self.build_operator_packet_by_length()
        else:
            return self.build_operator_packet_by_number()

    def build_operator_packet_by_length(self):
        version = ba_util.ba2int(self.bits[0:3])
        type_id = ba_util.ba2int(self.bits[3:6])
        self.bits = self.bits[7:]

        length = ba_util.ba2int(self.bits[0:15])
        self.bits = self.bits[15:]

        subconstructor = PacketConstructor(self.bits[0:length])
        subpackets = []
        while True:
            sp = subconstructor.build_packet()
            if sp == None:
                break
            subpackets.append(sp)

        self.bits = self.bits[length:]
        return OperatorPacket(version, type_id, subpackets)


    def build_operator_packet_by_number(self):
        version = ba_util.ba2int(self.bits[0:3])
        type_id = ba_util.ba2int(self.bits[3:6])
        self.bits = self.bits[7:]

        number = ba_util.ba2int(self.bits[0:11])
        self.bits = self.bits[11:]

        subpackets = []
        for i in range(number):
            subpackets.append(self.build_packet())
        return OperatorPacket(version, type_id, subpackets)