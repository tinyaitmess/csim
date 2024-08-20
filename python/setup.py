from distutils.core import setup, Extension

module = Extension(
	"csim",
	sources = ["python.c"],
	include_dirs = [".."],
	libraries = ["csim", "arm"],
	library_dirs = ["..", "../armv5t/src"]
)

setup(
	name = "csim",
	version = "1.0",
	description = "Component Simulation",
	ext_modules = [module]
)
