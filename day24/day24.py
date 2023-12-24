import numpy as np

sample_input = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"""


with open("input.txt") as t:
    sample_input = t.read()


"""
  t1      |    t2
19, -2    |  18, -1    =>  2(t1), -t2 = (19 - 18)
13, 1     |  19, -1    =>  -1(t1), -t2 = (13 - 19)

```python
a = n.array([[2, -1], [-1, -1]])
b = n.array([1, -6])
x = n.linalg.solve(a, b)
> array([2.33333333, 3.66666667]) -> [t1, t2]

| 19 + -2(t1) => X
| 18 + -1(t2) => X

| 13 + 1(t1) => Y

```

x, vx     | x', vx'    =>  -vx, vx' = (x - x')
y, vy     | y', vy'    =>  -vy, vy' = (y - y')

Hailstone A: 18, 19, 22 @ -1, -1, -2
Hailstone B: 20, 25, 34 @ -2, -2, -4

  t1     |   t2
18 -1    |  20 -2      => 1(t1), -2(t2) = (18 - 20)
19 -1    |  25 -2      => 1(t1), -2(t2) = (19 - 25)

a = n.array([[1, -2], [1, -2])
b = n.array([-2, -6])

"""


lines = sample_input.splitlines()


# Linear Equation
class Equation:
    def __init__(self, a, b) -> None:
        # x(t) = a - b(t)
        self.a = a
        self.b = b

    def __repr__(self) -> str:
        return f"x(t) = {self.a} + {self.b}(t)"


def makeEquation(s) -> (Equation, Equation):
    # ONLY 1 LINE
    [pos, vel] = s.split("@")
    pos = [int(a) for a in pos.split(", ")]
    vel = [int(a) for a in vel.split(", ")]
    return (Equation(pos[0], vel[0]), Equation(pos[1], vel[1]))


rock_equations = []
for line in lines:
    eqx, eqy = makeEquation(line)
    print((eqx, eqy))
    rock_equations.append([eqx, eqy])
"""
  t1      |    t2
19, -2    |  18, -1    =>  2(t1), -t2 = (19 - 18)
13, 1     |  19, -1    =>  -1(t1), -t2 = (13 - 19)

```python
a = n.array([[2, -1], [-1, -1]])
b = n.array([1, -6])
x = n.linalg.solve(a, b)
> array([2.33333333, 3.66666667]) -> [t1, t2]
"""


def solve(eq1x, eq1y, eq2x, eq2y):
    x, vx = eq1x.a, eq1x.b
    y, vy = eq1y.a, eq1y.b

    x1, vx1 = eq2x.a, eq2x.b
    y1, vy1 = eq2y.a, eq2y.b

    a = np.array([[-vx, vx1], [-vy, vy1]])
    b = np.array([x - x1, y - y1])

    try:
        [t1, t2] = np.linalg.solve(a, b)
        if not (t1 > 0 and t2 > 0):
            return False
        # print(f"TIME IS ! {t1=} {t2=}")
    except:
        # print("SKIPPING PARALLED EQ !")
        return False

    collison_x = x + (t1 * vx)
    collision_y = y + (t1 * vy)

    # print(f"{collison_x=}, {collision_y=}")
    r = (
        collison_x >= 200000000000000
        and collison_x <= 400000000000000
        and collision_y >= 200000000000000
        and collision_y <= 400000000000000
    )
    # If collides inside field
    return r


count = 0

for r in range(0, len(rock_equations)):
    eq1 = rock_equations[r]
    eq1x = eq1[0]
    eq1y = eq1[1]
    for rr in range(r + 1, len(rock_equations)):
        eq2 = rock_equations[rr]

        eq2x = eq2[0]
        eq2y = eq2[1]

        result = solve(eq1x, eq1y, eq2x, eq2y)

        if result:
            count += 1


print(count)


"""
x, vx     | x', vx'    =>  -vx, vx' = (x - x')
y, vy     | y', vy'    =>  -vy, vy' = (y - y')
"""

# 19 + -2(t) = 18 + -1(t')
# a + b(t) = a' + b'(t')

# >>> (1) -> b(t) - b'(t') = a - a'  (X coord eq)
# t = ((a - a') + b'(t')) / b => 7/3 -> 2.33333333333333333

# >>> (2) -> b(t) - b'(t') = a - a'  (Y coord eq)

#

# def comparseEquations()
