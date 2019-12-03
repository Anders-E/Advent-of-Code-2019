from sys import stdin
from collections import defaultdict

dirs = {"U": (0, 1), "R": (1, 0), "D": (0, -1), "L": (-1, 0)}

# Wire 1
wire1 = defaultdict(int)
pos = (0, 0)
dist = 0
for seg in stdin.readline().split(","):
    dir = dirs[seg[0]]
    length = int(seg[1:])
        
    for _ in range(length):
        dist += 1
        pos = (pos[0] + dir[0], pos[1] + dir[1])
        wire1[pos] = dist

# Wire 2
pos = (0, 0)
intersections = []
dist = 0
for seg in stdin.readline().split(","):
    dir = dirs[seg[0]]
    length = int(seg[1:])
    
    for _ in range(length):
        dist += 1
        pos = (pos[0] + dir[0], pos[1] + dir[1])
        if wire1[pos]:
            intersections.append(wire1[pos] + dist)

print(min(intersections))
