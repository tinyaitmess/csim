
PYTHON = $(shell which python3)

export PYTHONPATH=$(PWD)/../Orchid

NUM = 1

all:

run:
	$(PYTHON) run.py ../samples/sample$(NUM).yaml ../samples/sample$(NUM).elf

setup:
	$(PYTHON) setup.py install --user
