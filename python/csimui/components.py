"""Components of csimui"""

import csim
import yaml
from csimui import util

CSIM_SIMPLE = 1
CSIM_CORE = 2
CSIM_IO = 3

class Component:
	"""Represents a simple component."""

	def __init__(self, board, name, comp, inst, atts):
		self.board = board
		self.name = name
		self.comp = comp
		self.inst = inst

class Core(Component):
	"""Represents a core component."""

	def __init__(self, board, name, comp, inst, atts):
		Component.__init__(self, board, name, comp, inst, atts)
		self.core = csim.get_core(board.board)
		assert self.core is not None

	def load(self, path):
		"""Load the binary from the path."""
		csim.core_load(self.core, path)

	def pc(self):
		"""Get the current PC."""
		return csim.core_pc(self.core)

	def disasm(self, addr):
		"""Disassemble the given address."""
		return csim.core_disasm(self.core, addr)

class IOComponent(Component):
	"""Represents an IO component."""

	def __init__(self, board, name, comp, inst, atts):
		Component.__init__(self, board, name, comp, inst, atts)
		board.io_components.append(self)

	def install(self, canvas):
		"""Called at UI start time to let the IO component install itself
		in the canvas. The default implementation does nothing."""
		pass

	def update(self):
		"""Called each time the display needs to be updated for the
		current component."""
		pass

def make_io(board, name, comp, inst, atts):
	type = util.get(atts, "type", None)
	assert type is not None
	try:
		mod = __import__("csimui.%s" % type, fromlist=["csimui"])
	except ImportError as e:
		raise util.BoardError("cannot load %s: %s" % (type, e))
	return mod.Component(board, name, comp, inst, atts)

COMPONENTS = {
	CSIM_SIMPLE: Component,
	CSIM_CORE: Core,
	CSIM_IO: make_io
}

class Board:

	def run(self, time = 10):
		csim.run(self.board, time)

	def __init__(self, board_path, bin_path=None):
		self.board_path = board_path
		self.bin_path = bin_path
		self.components = []
		self.io_components = []

		# load the board
		with open(board_path, "r") as input:
			desc = yaml.safe_load(input)
		board_name = desc["name"]

		# build the board
		self.board = csim.new_board(board_name, None)
		self.core = None
		self.clock = util.get(desc, "clock", 1000)
		self.quantum = util.get(desc, "quantum", 100)
		if self.clock // self.quantum != self.clock / self.quantum:
			util.warn("quantum (%d) must be a divider of master clock(%dHz)" % (self.quantum, self.clock))
		csim.set_master_clock(self.board, self.clock)

		# build the components
		comps = util.obtain(desc, "components", "no component defined")
		for (name, cdesc) in comps.items():
			type = util.obtain(cdesc, "type", "no type defined for %s" % name)
			comp = csim.find_component(type)
			if comp is None:
				raise BoardError("cannot find component %s" % type)
			info = csim.component_info(comp)
			base = int(util.get(cdesc, "base", "0"), 16)
			inst = csim.new_component(self.board, comp, name, base)
			ctype = info[1]
			obj = COMPONENTS[ctype](self, name, comp, inst, cdesc)
			self.components.append(obj)
			if isinstance(obj, Core):
				if self.core != None:
					raise BoardError("several cores defined!")
				else:
					self.core = obj

		# build the connections
		cons = util.get(desc, "connect")
		if cons != None:
			for con in cons:
				from_ = util.obtain(con, "from", "no 'from' in connection")
				(from_inst, from_port) = self.parse_port(from_)
				to_ = util.obtain(con, "to", "no 'to' in connection")
				(to_inst, to_port) = self.parse_port(to_)
				csim.connect(from_inst, from_port, to_inst, to_port)

		# check for core
		if self.core is None:
			raise util.BoardError("no core defined!")

		# load the code
		if bin_path is not None:
			self.load_bin(bin_path)

	def load_bin(self, path):
		"""Load the binary in memory."""
		self.core.load(bin_path)

	def parse_port(self, text):
		both = text.split('.')
		if len(both) != 2:
			raise util.BoardError("port format must be COMPONENT.BOARD!")
		found_inst = None
		for comp in self.components:
			if comp.name == both[0]:
				found_inst = comp
				break
		if found_inst is None:
			raise util.BoardError("cannot find instance '%s'!" % both[0])
		port = csim.find_port(found_inst.comp, both[1])
		if port == None:
			raise util.BoardError("cannot find port '%s' in '%s'" % (both[1], both[0]))
		return (found_inst.inst, port)


	def update(self):
		"""Called each the IO components needs to be updated."""
		for comp in self.io_components:
			comp.update()
