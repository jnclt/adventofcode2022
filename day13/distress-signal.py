#!/usr/bin/env python3

import sys
from itertools import zip_longest
from typing import Tuple

def main(input_path: str):
    with open(input_path) as f:
        lines = f.readlines()
        pairs = zip(map(eval, lines[0::3]), map(eval, lines[1::3]))
    indices = [i + 1 for  i, v in enumerate([is_ordered(*p) for p in pairs]) if v]
    print(sum(indices))

def is_ordered(l:list|int, r:list|int ) -> bool|None:
  match (l, r):
    case (int(), int()) if l == r: return None
    case (int(), int()) if l != r: return l < r
    case (list(), list()):
        lists = zip_longest(l, r, fillvalue=-1)
        for (ll, rr) in lists:
            order = is_ordered(ll, rr)
            if order is not None:
                return order
        return None
    case (int(), list()): return is_ordered([l], r)
    case (list(), int()): return is_ordered(l, [r])


if __name__ == "__main__":
    main(sys.argv[1])