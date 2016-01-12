#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import time


def draw(filename, begin, end):
    printing = False
    with open(filename) as f:
        for line in f:
            if line.strip() == 'Step: {0}'.format(begin):
                printing = True
                sys.stdout.write(line)
                continue
            if line.strip() == 'Step: {0}'.format(end):
                break
            if printing:
                if line.startswith('Step'):
                    sys.stdout.flush()
                    time.sleep(0.2)
                    os.system('clear')
                sys.stdout.write(line)


if __name__ == '__main__':
    draw(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]) + 1)
