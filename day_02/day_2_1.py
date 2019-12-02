from sys import stdin
from operator import add, mul

prog = list(map(int, stdin.read().split(",")))
op = {1: add, 2: mul}

prog[1] = 12
prog[2] = 2

for pos in range(0, len(prog), 4):
    a, b, c, d = prog[pos:pos+4]
    if a == 99: break
    prog[d] = op[a](prog[b], prog[c])

print(prog[0])
