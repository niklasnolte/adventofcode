import numpy as np
from numpy import ma


def read_random_numbers(txt):
    return [int(n) for n in txt.split(",")]


def read_bingo_line(line):
    return list(map(int, line.split()))


def read_bingo_boards(lines):
    lines = [read_bingo_line(l) for l in lines if l != "\n"]
    return np.array(lines).reshape(-1, 5, 5)


def get_winner_score(rns, bs):
    tags = np.zeros_like(bs, dtype=bool)
    for rn in rns:
        tags |= bs == rn
        for i, t in enumerate(tags):
            if t.all(axis=1).any() or t.all(axis=0).any():
                return rn * sum(bs[i][~t])


def get_loser_score(rns, bs):
    havent_won = np.ones(len(bs), dtype=bool)
    bs = ma.array(bs, mask=np.zeros_like(bs))
    for rn in rns:
        bs = bs[havent_won]
        havent_won = havent_won[havent_won]
        bs[bs == rn] = ma.masked
        for i, t in enumerate(bs.mask):
            if t.all(axis=1).any() or t.all(axis=0).any():
                havent_won[i] = False
            if len(bs.mask) == 1 and havent_won[0] == False:
                return rn * bs[i].sum()


def main():
    with open("input4.txt") as f:
        lines = f.readlines()
    random_numbers = read_random_numbers(lines[0])
    boards = read_bingo_boards(lines[1:])
    print(get_winner_score(random_numbers, boards))
    print(get_loser_score(random_numbers, boards))


main()
