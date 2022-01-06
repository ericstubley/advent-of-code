import bitarray as ba
import bitarray.util as ba_util


class Circuit:
    def __init__(self):
        self.wires = []
        self.outputs = dict()
        self.gate_types = dict()
        self.gate_data = dict()

    def add_wire(self, wire, gate_type, gate_datum):
        self.wires.append(wire)
        self.outputs[wire] = None
        self.gate_types[wire] = gate_type
        self.gate_data[wire] = gate_datum

    def output_to_int(self, wire):
        return ba_util.ba2int(self.outputs[wire])

    def compute_outputs(self):
        for w in self.wires:
            if self.outputs[w] is None:
                self.compute_output(w)

    def compute_output(self, wire):
        if wire not in self.wires:
            return ba_util.int2ba(int(wire), 16)

        if self.outputs[wire] is None:

            gate_type = self.gate_types[wire]

            output = None

            if gate_type == "AND":
                w1, w2 = self.gate_data[wire]
                output = self.compute_output(w1) & self.compute_output(w2)
            elif gate_type == "OR":
                w1, w2 = self.gate_data[wire]
                output = self.compute_output(w1) | self.compute_output(w2)
            elif gate_type == "NOT":
                w = self.gate_data[wire]
                output = ~self.compute_output(w)
            elif gate_type == "LSHIFT":
                w, shift = self.gate_data[wire]
                output = self.compute_output(w) << shift
            elif gate_type == "RSHIFT":
                w, shift = self.gate_data[wire]
                o = self.compute_output(w)
                output = self.compute_output(w) >> shift
            elif gate_type == "SIGNAL":
                signal = self.gate_data[wire]
                output = self.compute_output(signal)

            self.outputs[wire] = output

        return self.outputs[wire]


class CircuitBuilder:
    def __init__(self):
        self.circuit = Circuit()

    def build_circuit_from_list(self, instructions):
        for i in instructions:
            destination_wire = i.rstrip('\n').split(" -> ")[1]
            gate_info = i.split(" -> ")[0]

            if "AND" in gate_info:
                gate_type = "AND"
                gate_datum = gate_info.split(" AND ")

            elif "OR" in gate_info:
                gate_type = "OR"
                gate_datum = gate_info.split(" OR ")

            elif "NOT" in gate_info:
                gate_type = "NOT"
                gate_datum = gate_info.lstrip("NOT ")

            elif "LSHIFT" in gate_info:
                gate_type = "LSHIFT"
                gate_datum = gate_info.split(" LSHIFT ")
                gate_datum[1] = int(gate_datum[1])

            elif "RSHIFT" in gate_info:
                gate_type = "RSHIFT"
                gate_datum = gate_info.split(" RSHIFT ")
                gate_datum[1] = int(gate_datum[1])

            else:  # it should be a straight signal
                gate_type = "SIGNAL"
                gate_datum = gate_info

            self.circuit.add_wire(destination_wire, gate_type, gate_datum)
