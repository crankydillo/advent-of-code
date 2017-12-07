# Developed with python 2.7
# registers = [0, 2, 7, 0]

# Can't use lists in sets and what happens when you add a tuple to a set makes
# no sense to me.  Switching to Scala and will come back to this if there's
# time.
#
# Furthermore, after [completing](../scala) the exercise with Scala, I
# discovered the algorithm below is not correct.  I misunderstood the problem.
registers = (2, 2, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14)

previous = set(registers)

print previous

def redistribute(registers):
    # Not efficient
    max_val = max(registers)
    biggest_idx = registers.index(max_val)
    num_registers = len(registers)
    redistribution_amt = max_val // (num_registers - 1)
    biggest_idx_val = max_val % (num_registers - 1)
    new_registers = [0] * num_registers
    new_registers[biggest_idx] = biggest_idx_val
    for idx, val in enumerate(registers):
        if (idx != biggest_idx):
            new_registers[idx] = val + redistribution_amt
    return new_registers

ctr = 1

while True:
    registers = redistribute(registers)
    registers_tuple = tuple(registers)
    if (registers_tuple in previous):
        break
    previous.add(registers_tuple)
    ctr = ctr + 1
print previous
print ctr

