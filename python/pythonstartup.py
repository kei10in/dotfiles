# -*- coding: utf-8 -*-

import sys
import os
import io
import itertools
import functools

import readline
import rlcompleter
import atexit


# allow tab completion
readline.parse_and_bind('tab: complete')

# set history file
histfile = os.path.join(os.environ['HOME'], '.pythonhistory')
try:
    readline.read_history_file(histfile)
except IOError:
    pass
atexit.register(readline.write_history_file, histfile)


del atexit, rlcompleter, readline
