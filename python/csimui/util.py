"""Uitlity definitions for csimui module."""

import sys
from orchid import Buffer

# Error management
def error(msg):
	print("ERROR:", msg)

def fatal(msg):
	error(msg)
	sys.exit(1)

class BoardError(Exception):

	def __init__(self, msg):
		self.msg = msg

	def __str__(self):
		return self.msg


# Map access
def get(map, name, default = None):
	try:
		return map[name]
	except KeyError:
		return default

def obtain(map, name, msg):
	try:
		return map[name]
	except KeyError:
		raise BoardError(msg)


# SVG
def load_svg(path):
	buf = Buffer()
	with open(path) as input:
		l = input.readline()
		while not l.startswith("<svg"):
			l = input.readline()
		while not l.endswith(">\n"):
			l = input.readline()
		l = input.readline()
		while not l.startswith("</svg>"):
			buf.write(l)
			l = input.readline()
	return str(buf)
