#! /usr/bin/env python3

# from automation.automation import submit_answer


def parse_turing_machine(filename):
    with open(filename) as f:
        data = f.readlines()


    initial_state = data[0].split()[-1].rstrip('.')
    diagnostic_time = int(data[1].split()[-2])


    states = dict()
    for i in range(3, len(data), 10):
        state = data[i].split()[-1].rstrip(':')

        t0 = parse_instruction(data[i+2:i+5])
        t1 = parse_instruction(data[i+6:i+9])

        states[state] = [t0, t1]

    return TuringMachine(states, initial_state, diagnostic_time)


def parse_instruction(lines):
    write_value = int(lines[0].split()[-1].rstrip('.'))
    direction = -1 if lines[1].split()[-1].rstrip('.') == "right" else 1
    next_state = lines[2].split()[-1].rstrip('.')

    return (write_value, direction, next_state)


class TuringMachine:
    def __init__(self, states, initial_state, diagnostic_time):
        self.states = states
        self.tape = 0
        self.curr_state = initial_state
        self.curr_index = 0
        self.diagnostic_time = diagnostic_time

    def diagnostic_checksum(self):
        for t in range(self.diagnostic_time):
            # if t % 100000 == 0:
            #     print(t//100000, self.tape.bit_length())
            self.advance()

        return self.num_bits()


    def advance(self):
        curr_bit = (self.tape >> self.curr_index) & 1


        write, move, next_state = self.states[self.curr_state][curr_bit]

        # update the tape
        if curr_bit == write:
            pass
        else:
            # toggle the current bit
            self.tape = self.tape ^ (1 << self.curr_index) 

        # move the head
        if move == 1:
            self.curr_index += 1
        elif move == -1 and self.curr_index == 0:
            self.tape <<= 1
        else:  # move == -1 and self.curr_index != 0
            self.curr_index -= 1


        self.curr_state = next_state

    def num_bits(self):
        # count the number of bits which are set in self.tape
        return bin(self.tape)[2:].count('1')


def main_a(tm):
    answer = tm.diagnostic_checksum()
    print(answer)
    # result = submit_answer(2017, 25, 1, answer)
    # print(result)


def main_b():
    pass


if __name__ == "__main__":
    tm = parse_turing_machine("input.txt")
    main_a(tm)
    main_b()
