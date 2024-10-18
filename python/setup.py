#!/usr/bin/python3

import os.path
from distutils.core import setup, Extension

lib_dirs = [".."]
with open("../config.mk") as input:
	for line in input.readlines():
		path = line.strip()[12:]
		if line.startswith("WITH_ARMV5T="):
			lib_dirs.append(os.path.join("..", path, "src"))

module = Extension(
	"csim",
	sources = ["python.c"],
	include_dirs = [".."],
	libraries = ["csim", "arm"],
	library_dirs = lib_dirs
)

setup(
	name = "csim",
	version = "1.0",
	description = "Component Simulation",
	ext_modules = [module]
)
