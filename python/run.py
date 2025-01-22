#!/usr/bin/env python3

import sys
from csimui.util import *
from csimui.components import *
from csimui.app import *

# parse arguments
if len(sys.argv) != 3:
	fatal("requires argument <board> <binary>")

board_path = sys.argv[1]
bin_path = sys.argv[2]

# main program
try:
	board = Board(board_path, bin_path)
	MyApp(board).run(debug=False)
except BoardError as e:
	print("ERROR:", str(e))
