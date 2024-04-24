"""Application for csimui."""

from orchid import *
from orchid import svg

class MyPage(Page):

	def __init__(self, app, board):
		self.board = board
		self.addr = Label("XXXX XXXXX")
		self.inst = Label("NOP")
		self.run_but = Button("Run", on_click=self.run)
		self.stop_but = Button("Stop", on_click=self.stop, enabled=False)
		self.step_but = Button("Step", on_click=self.step)
		self.canvas = svg.Canvas()
		for io in board.io_components:
			io.install(self.canvas)
		Page.__init__(
			self,
			VGroup([
				HGroup([
					self.run_but,
					self.stop_but,
					self.step_but,
					self.addr,
					self.inst
				]),
				self.canvas
			]),
			app = app
		)
		self.show_current()
		self.timer = Timer(self, trigger=self.run_once, period=100)

	def show_current(self):
		addr = self.board.core.pc()
		self.addr.set_text("%08x" % addr)
		text = self.board.core.disasm(addr)
		self.inst.set_text(text)

	def run(self):
		self.timer.start()
		self.run_but.disable()
		self.step_but.disable()
		self.stop_but.enable()

	def stop(self):
		self.timer.stop()
		self.run_but.enable()
		self.step_but.enable()
		self.stop_but.disable()

	def step(self):
		self.board.run(1)
		self.show_current()

	def run_once(self):
		self.board.run()
		self.show_current()
		self.board.update()


class MyApp(Application):

	def __init__(self, board):
		Application.__init__(self, "CSIM")
		self.fst = MyPage(self, board)

	def first(self):
		return self.fst

	def run(self):
		run(self)
