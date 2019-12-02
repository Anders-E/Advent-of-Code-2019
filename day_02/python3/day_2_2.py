from sys import stdin
from operator import add, mul

program = list(map(int, stdin.read().split(",")))
op = {1: add, 2: mul}

for noun in range(100):
    for verb in range(100):
        prog = program.copy()
        prog[1] = noun
        prog[2] = verb
        for pos in range(0, len(prog), 4):
            a, b, c, d = prog[pos:pos+4]
            if a == 99: break
            prog[d] = op[a](prog[b], prog[c])
        if prog[0] == 19690720:
            print(100 * noun + verb)
            exit()
