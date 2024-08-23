#!/usr/bin/env python3

import sys
import csim

# prepare the simulator
board = csim.load_board("../samples/sample1.yaml")
print(board)
core = csim.get_core(board)
print(core)
rc = csim.core_load(core, "../samples/sample1.elf")

# get registers
core_comp = csim.find_component("arm")
(name, type, version, reg_cnt, port_cnt, size) = csim.component_info(core_comp)
print("DEBUG:", name, type, version, reg_cnt, port_cnt, size)

core_inst = csim.core_inst(core)
for i in range(reg_cnt):
	reg = csim.get_register(core_comp, i)
	print(reg)
	(name, offset, size, count, stride, flags, type) = csim.register_info(reg)
	print("DEBUG: reg =", name, offset, size, count, stride, flags, type)
	for j in range(count):
		print("DEBUG:", name, j, csim.get_register_val(core_inst, reg, j))
		csim.set_register_val(core_inst, reg, j, 0)


sys.exit(0)

# run it
print("Started!")
for i in range(0, 20):
	pc = csim.core_pc(core)
	print("%08x: %s" % (pc, csim.core_disasm(core, pc)))
	csim.run(board, 1)

# free it
print("Ending!")
csim.delete_board(board)
