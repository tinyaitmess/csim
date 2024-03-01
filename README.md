# GLISS Components

Small program demonstrating the use of CSim with ARMv5T simulator.

## Building

To compile it, you need a compiled version of ARMv5T compiled in a directory in the same parent as `csim`. The path of ARMV5T can also be fixed in variable `ARMV5T` in the `Makefile`.

Then type the command:

```sh
	$ make
```

To get ARMV5T, you have to first get GLISS2 (OCAML is required):

```sh
	$ git clone https://git.renater.fr/anonscm/git/gliss2/gliss2.git
	$ cd gliss2
	$ make
```

Then you can retrieve and compiole `armv5r` :

```sh
	$ git clone https://git.renater.fr/anonscm/git/gliss2/armv5t.git
	$ cd armv5t
	$ make
```

**These directories have to be unpacked in the same parent directory.**



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

* `sample2.elf` (source `sample2.s`) -- Invert the state of the LEAD each time there is a click (a button push followed by a release).
