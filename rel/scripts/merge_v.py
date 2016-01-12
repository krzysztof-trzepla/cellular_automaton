#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def merge(filename1, filename2, filename):
    with open(filename1) as f:
        lines1 = f.readlines()
    with open(filename2) as f:
        lines2 = f.readlines()

    lines = []
    for i, l in enumerate(lines1):
        if l.startswith('Step:'):
            lines.append(l)
        else:
            lines.append(l.strip() + lines2[i])

    with open(filename, 'w') as f:
        f.writelines(lines)


if __name__ == '__main__':
    merge(sys.argv[1], sys.argv[2], sys.argv[3])
