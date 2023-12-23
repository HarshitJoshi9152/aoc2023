# Could i have used dijkstra for this ?
import sys
from typing import TypeAlias

from enum import Enum

sys.setrecursionlimit(10**4)


with open("./input.txt") as i:
    contents = i.read()

MAZE = contents.splitlines()

MAZE_HEIGHT = len(MAZE)
MAZE_WIDTH = len(MAZE[0])

START_TILE = (0, MAZE[0].find("."))
END_TILE = (len(MAZE) - 1, MAZE[-1].find("."))

WALKED_PATHS = set()

Coordinate: TypeAlias = tuple[int, int]


class Direction(Enum):
    UP = 1
    DOWN = 2
    LEFT = 3
    RIGHT = 4


class NextPath:
    def __init__(self, coord: Coordinate, dir: Direction):
        self.coord = coord
        self.dir = dir

    def canGo(self, cameFrom: Direction):
        return all([not (self.dir == cameFrom), self.inMaze(), self.isValidTile()])

    def inMaze(self):
        r, c = self.coord
        # I shoulnt be calling the length function here lol
        return r > -1 and r < MAZE_HEIGHT and c > -1 and c < MAZE_WIDTH

    def isValidTile(self):
        r, c = self.coord
        return MAZE[r][c] != "#" and (not self.coord in WALKED_PATHS)


def travel(tileCoord, cameFrom: Direction):
    if tileCoord == END_TILE:
        # print("REACHED !")
        return [END_TILE]
    else:
        WALKED_PATHS.add(tileCoord)
        r, c = tileCoord

        up, down, left, right = ((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))

        paths = [
            NextPath(up, Direction.UP),
            NextPath(down, Direction.DOWN),
            NextPath(left, Direction.LEFT),
            NextPath(right, Direction.RIGHT),
        ]

        # IF we land on a slope we must follow that direction
        tile = MAZE[r][c]
        if tile in [">", "<", "^", "v"] and r == 18 and c == 13:
            pass
        # COMMENTED FOR PART 2
        # if tile == ">":
        #     paths = [NextPath(right, Direction.RIGHT)]
        # elif tile == "<":
        #     paths = [NextPath(left, Direction.LEFT)]
        # elif tile == "^":
        #     paths = [NextPath(up, Direction.UP)]
        # elif tile == "v":
        #     paths = [NextPath(down, Direction.DOWN)]

        maxPath = []
        for p in paths:
            if p.canGo(
                cameFrom
            ):  # I can probably reduce this to a clever list comprehension
                dirFrom = None
                if p.dir == Direction.UP:
                    dirFrom = Direction.DOWN
                elif p.dir == Direction.DOWN:
                    dirFrom = Direction.UP
                elif p.dir == Direction.LEFT:
                    dirFrom = Direction.RIGHT
                elif p.dir == Direction.RIGHT:
                    dirFrom = Direction.LEFT

                print(f"TRAVELLING {p.dir} TO > ", p.coord)
                foundPath = travel(p.coord, dirFrom)
                if len(maxPath) < len(foundPath):
                    maxPath = foundPath

        # When one path has been evaluated, clear the WALKED_TILES for other path evaluations
        WALKED_PATHS.remove(tileCoord)
        return [tileCoord] + maxPath


result = travel(START_TILE, Direction.UP)

for ri, r in enumerate(MAZE):
    for ci, c in enumerate(r):
        if (ri, ci) in result:
            print("O", end="")
        else:
            print("-" if (c == "#") else c, end="")
    print()

print(len(result) - 1)
