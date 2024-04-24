
import os.path
from orchid.svg import Content
from csimui import components
from csimui.util import *
import csim

class Component(components.IOComponent):
	IMAGE = None

	def __init__(self, board, name, comp, inst, atts):
		components.IOComponent.__init__(self, board, name, comp, inst, atts)
		self.x = get(atts, "x", 0)
		self.y = get(atts, "y", 0)

	def on_push(self):
		self.canvas.get_page().set_direct_attr(
			"%s-push" % self.content.get_id(), "fill", "#888888")
		csim.set_state(self.inst, [1])

	def on_release(self):
		self.canvas.get_page().set_direct_attr(
			"%s-push" % self.content.get_id(), "fill", "#000000")
		csim.set_state(self.inst, [0])

	def install(self, canvas):
		self.canvas = canvas

		# load image
		if Component.IMAGE == None:
			Component.IMAGE = load_svg(os.path.join(os.path.dirname(__file__), "button.svg"))

		# build the UI
		self.content = canvas.content(Component.IMAGE)
		self.content.scale(2)
		self.content.translate(self.x, self.y)
		print(self.content)
		self.content.add_event("onmousedown", self.on_push)
		self.content.add_event("onmouseup", self.on_release)
