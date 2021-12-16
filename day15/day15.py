import sys
import copy
memo = {}

with open("input.txt") as f:
    lines = f.read().splitlines()
    grid = [[int(c) for c in line] for line in lines]

maxX = len(grid)
maxY = len(grid[0])
print(maxX)
print(maxY)

def neighbors(x,y):
    n = []
    if x > 0:
        n.append((x-1, y))
    if y > 0:
        n.append((x,y-1))
    if y < maxY - 1:
        n.append((x,y+1))
    if x < maxX - 1:
        n.append((x+1,y))
    return n

def a_star(grid):
    distances = [[float("inf") for x in range(maxY)] for y in range(maxX)]
    distances[0][0] = 0
    visited = [[False for x in range(maxY)] for y in range(maxX)]
    priorities = {}
    priorities[(0,0)] = maxX + maxY

    while True:
        lowest_priority = float("inf")
        (lx,ly) = (None,None)
        for (x,y) in priorities:
            if priorities[(x,y)] < lowest_priority and not visited[x][y]:
                lowest_priority = priorities[(x,y)]
                (lx,ly) = (x,y)

        if lx is None:
            return -1
        elif (lx,ly) == (maxX-1, maxY-1):
            return distances[x][y]

        print(f"Visiting node ({lx},{ly}) with currently lowest priority of {lowest_priority}")

        n = neighbors(lx,ly)
        for (x,y) in n:
            if not visited[x][y]:
                if distances[lx][ly] + grid[x][y] < distances[x][y]:
                    distances[x][y] = distances[lx][ly] + grid[x][y]
                    priorities[(x,y)] = distances[x][y] + (maxX - x) + (maxY - y)

        # Lastly, note that we are finished with this node.
        visited[lx][ly] = True
        del priorities[(lx,ly)]

def extendDown(grid):
    for i in range(4):
        for j in range(maxX):
            row = copy.deepcopy(grid[j + i * maxX])
            for k in range(len(row)):
                row[k] = row[k]+1 if row[k] < 9 else 1
            grid.append(row)

def extendRight(grid):
    for i in range(len(grid)):
        row = grid[i]
        for j in range(maxY, 5 * maxY):
            val = row[j-maxY] + 1 if row[j-maxY] < 9 else 1
            row.append(val)

memo = {}
smart = a_star(grid)
print(smart)

extendDown(grid)
extendRight(grid)
maxX = len(grid)
maxY = len(grid[0])
smart = a_star(grid)
print(smart)
