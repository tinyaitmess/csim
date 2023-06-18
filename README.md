# GLISS Components

Small program demonstrating the use of CSim with ARMv5T simulator.

## Building

To compile it, you need a compiled version of ARMv5T compiled in a directory in the same parent as `csim`. The path of ARMV5T can also be fixed in variable `ARMV5T` in the `Makefile`.

Then type the command:

	$ make


## Using it

To launch the code, a program must be provided to the command, for instance `samples/sample1.elf`. Then type:

	$ ./test2 samples/sample1.elf

The current simulation simulates the program with two peripherals: a LED and a push button. They are displayed this way:

	[ ] (A) HHHHHHHH <inst>

Where:

* `[ ]` switched-off LED (`[*]` when it is switched on).
* `(A)` is the unpressed push button A. To press it, click on key `A`. To release it, re-click on key `A`.
* _HHHHHHHH_ is the adress of the current instruction.
* _<inst>_ is the disassembled current instruction.


## Programs

* `sample1.elf` (source `sample1.s`) -- Switch on the LED when the button is pushed.

