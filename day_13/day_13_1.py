import subprocess

proc = subprocess.Popen(["./interpreter"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

m = {}

out = list(map(int, proc.stdout))
for i in range (0, len(out) - 2, 3):
    x = out[i]
    y = out[i+1]
    id = out[i+2]
    m[(x, y)] = id

print(len(list(filter(lambda x: x == 2, m.values()))))
