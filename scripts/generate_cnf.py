from sys import argv
import json
from typing import Tuple, List
from math import sqrt

def generate_cnf(points: list):
    cnf = []
    numbers = [i for i in range(1, len(points) + 1)]

    # permissive clauses
    for point in points:
        clause = [(num, point) for num in numbers]
        cnf.append(clause)

    # point restrictive clauses
    for point in points:
        for (i, num_i) in enumerate(numbers):
            for (j, num_j) in enumerate(numbers):
                if i >= j:
                    continue

                clause = [(-1 * num_i, point), (-1 * num_j, point)]
                cnf.append(clause)

    # number restrictive clauses
    for num in numbers:
        for (i, point_i) in enumerate(points):
            for (j, point_j) in enumerate(points):
                if i >= j:
                    continue

                clause = [(-1 * num, point_i), (-1 * num, point_j)]
                cnf.append(clause)

    return cnf



def decode_assignment(assignment: int, bit_to_value: list):
    clause = []

    for bit in range(0, 9):
        if assignment & (0b1 << bit) != 0:
            clause.append(bit_to_value[bit])
        else:
            (num, point) = bit_to_value[bit]  
            clause.append((-1 * num, point))

    return clause[::-1]


def test_assignment(assignment, cnf) -> bool:
    var_to_asg = {(abs(num), point): num // abs(num) for (num, point) in assignment}

    for clause in cnf:
        clause_satisfied = False
        for (num, point) in clause:
            value = num // abs(num)
            key = (abs(num), point)

            if var_to_asg[key] == value:
                clause_satisfied = True

        if not clause_satisfied:
            return False

    return True

# this check is only for the 3 class. By induction, it should work on n >= 3
# n = 3, therefore test 512 different combinations.
# the bits are divided as such:
#
# 1a 1b 1c 2a 2b 2c 3a 3b 3c
# 0  0  0  0  0  0  0  0  0 
#
# so, the number 100 maps to
# 1a 1b 1c 2a 2b 2c 3a 3b 3c
# 0  0  1  1  0  0  1  0  0
# 
# which should evaluate to false
def assert_3_valid():
    cnf = generate_cnf(["a", "b", "c"])

    bit_to_value = [
        (1, "a"),
        (1, "b"),
        (1, "c"),
        (2, "a"),
        (2, "b"),
        (2, "c"),
        (3, "a"),
        (3, "b"),
        (3, "c"),
    ][::-1]

    expected_valid = sorted([
        0b100010001,
        0b100001010,
        0b010100001,
        0b010001100,
        0b001010100,
        0b001100010
    ])

    test_valid = []


    for assignment in range(0, 512):
        clause = decode_assignment(assignment, bit_to_value)
        satisfies = test_assignment(clause, cnf)

        if satisfies:
            test_valid.append(assignment)


    if sorted(test_valid) == expected_valid:
        print("The lists match")
    else:
        print("The lists don't match")


# given a point and the type of board, generate the id for this point
def point_identity(num: int, point: Tuple[int, int]) -> int:
    (row, col) = point

    if row < 0 or row >= num:
        raise Exception("row must satisfy 0 <= row < 9")
    
    if col < 0 or col >= num:
        raise Exception("col must satisfy 0 <= col < 9")

    return num * row + col

def gen_square_points(num: int, start: Tuple[int, int]) -> List[Tuple[int, int]]:
    (start_row, start_col) = start

    if start_row < 0 or start_row >= num:
        raise Exception("start_row must satisfy 0 <= start_row < 9")
    
    if start_col < 0 or start_col >= num:
        raise Exception("start_col must satisfy 0 <= start_col < 9")

    # this may produce some weird results when num isn't a perfect square 
    root = int(sqrt(num))

    points = []

    for row in range(start_row, start_row + root):
        for col in range(start_col, start_col + root):
            point = point_identity(num, (row, col))
            points.append(point)

    return points


def gen_row_points(num: int, start: Tuple[int, int]) -> List[Tuple[int, int]]:
    (start_row, start_col) = start

    if start_row < 0 or start_row >= num:
        raise Exception("start_row must satisfy 0 <= start_row < 9")
    
    if start_col != 0:
        raise Exception("start_col must start at 0")

    points = []

    for col in range(start_col, start_col + num):
        point = point_identity(num, (start_row, col))
        points.append(point)

    return points


def gen_col_points(num: int, start: Tuple[int, int]) -> List[Tuple[int, int]]:
    (start_row, start_col) = start

    if start_row != 0:
        raise Exception("columns must start at row 0.")
    
    if start_col < 0 or start_col >= num:
        raise Exception("start_col must satisfy 0 <= start_col < 9")

    points = []

    for row in range(start_row, start_row + num):
        point = point_identity(num, (row, start_col))
        points.append(point)

    return points


# generate cnf for num x num board
def generate_sudoku(num: int):

    full_cnf = []

    root = int(sqrt(num))

    # generate cnfs for all the squares
    for row in range(0, num, root):
        for col in range(0, num, root):
            points = gen_square_points(num, (row, col))
            cnf = generate_cnf(points)
            full_cnf += cnf
    
    # generate cnfs for all the rows
    for row in range(0, num):
        points = gen_row_points(num, (row, 0))
        cnf = generate_cnf(points)
        full_cnf += cnf

    # generate cnfs for all the columns
    for col in range(0, num):
        points = gen_col_points(num, (0, col))
        cnf = generate_cnf(points)
        full_cnf += cnf

    return full_cnf

if __name__ == "__main__": 
    if len(argv) != 3:
        raise Exception("Usage: python3 generate_cnf.py <num points> <file>")

    num_points = int(argv[1])
    file = argv[2]

    result = generate_sudoku(num_points)

    with open(file, 'w') as f:
        f.write(json.dumps(result))
