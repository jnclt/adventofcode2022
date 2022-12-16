#!/usr/bin/env python3

import sys
from functools import cmp_to_key
from typing import Tuple

def main(input_path: str):
    with open(input_path) as f:
        lines = f.readlines()
        pairs = zip(map(eval, lines[0::3]), map(eval, lines[1::3]))
    indices = [i + 1 for  i, v in enumerate([is_ordered(*p) for p in pairs]) if v]
    print(sum(indices))

    unordered = list(map(eval, filter(lambda l: l != "\n", lines))) + [[[2]], [[6]]]
    ordered = sorted(unordered, key=cmp_to_key(lambda l, r: -1 if is_ordered(l, r) else 1))
    print((ordered.index([[2]]) + 1) * (ordered.index([[6]]) + 1))

def is_ordered(l:list|int, r:list|int ) -> bool|None:
  match (l, r):
    case (int(), int()) if l == r: return None
    case (int(), int()) if l != r: return l < r
    case (list(), list()):
        lists = zip(l, r)
        for (ll, rr) in lists:
            order = is_ordered(ll, rr)
            if order is not None:
                return order
        if len(l) != len(r):
            return len(l) < len(r)
        return None
    case (int(), list()): return is_ordered([l], r)
    case (list(), int()): return is_ordered(l, [r])


if __name__ == "__main__":
    main(sys.argv[1])