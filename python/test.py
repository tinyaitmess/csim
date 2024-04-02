#!/usr/bin/env python3

import csim

# prepare the simulator
board = csim.load_board("../samples/sample1.yaml")
print(board)
core = csim.get_core(board)
print(core)
rc = csim.core_load(core, "../samples/sample1.elf")
print("rc =", rc)

# run it
for i in range(0, 20):
	pc = csim.core_pc(core)
	print("%08x: %s" % (pc, csim.core_disasm(core, pc)))
	csim.run(board, 1)

# free it
csim.delete_board(board)
