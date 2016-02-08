#!/usr/bin/env python3

import sys

out = open(sys.argv[3], 'w+')
out.write(sys.argv[1] + ' ')
out.write(sys.argv[2] + ' ')
out.write(sys.argv[3])
