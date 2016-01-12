#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def merge(filename1, filename2, filename):
    with open(filename1) as f:
        lines1 = f.readlines()
    with open(filename2) as f:
        lines2 = f.readlines()

    steps1 = []
    lines = [lines1[0]]
    for i in xrange(1, len(lines1)):
        l = lines1[i]
        if l.startswith('Step:'):
            steps1.append(lines)
            lines = [l]
        else:
            lines.append(l)

    steps2 = []
    lines = [lines2[0]]
    for i in xrange(1, len(lines2)):
        l = lines2[i]
        if l.startswith('Step:'):
            steps2.append(lines)
            lines = [l]
        else:
            lines.append(l)

    lines = []
    for i, l in enumerate(steps1):
        lines.extend(l)
        lines.extend(steps2[i][1:])

    with open(filename, 'w') as f:
        f.writelines(lines)


if __name__ == '__main__':
    merge(sys.argv[1], sys.argv[2], sys.argv[3])
