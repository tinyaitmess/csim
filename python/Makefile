
include ../config.mk

PYTHON = $(shell which python3)

export PYTHONPATH=../$(ORCHID_PATH)

NUM = 1

all:
	$(PYTHON) setup.py build

run:
	echo $(PYTHONPATH)
	$(PYTHON) run.py ../samples/sample$(NUM).yaml ../samples/sample$(NUM).elf

install:
	$(PYTHON) setup.py install --user


autodoc:
	gnome-terminal -- pydoc3 -b
