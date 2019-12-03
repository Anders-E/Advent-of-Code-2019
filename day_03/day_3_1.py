from sys import stdin

dirs = {"U": (0, 1), "R": (1, 0), "D": (0, -1), "L": (-1, 0)}

# Wire 1
wire1 = set()
pos = (0, 0)
for seg in stdin.readline().split(","):
    dir = dirs[seg[0]]
    length = int(seg[1:])

    for _ in range(length):
        pos = (pos[0] + dir[0], pos[1] + dir[1])
        wire1.add(pos)

# Wire 2
pos = (0, 0)
intersections = []
for seg in stdin.readline().split(","):
    dir = dirs[seg[0]]
    length = int(seg[1:])
    
    for _ in range(length):
        pos = (pos[0] + dir[0], pos[1] + dir[1])
        if pos in wire1:
            intersections.append(pos)

print(min([abs(x) + abs(y) for x, y in intersections]))
