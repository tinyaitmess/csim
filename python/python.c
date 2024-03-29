#define PY_SSIZE_T_CLEAN
#include <stdio.h>
#include <Python.h>
#include <csim.h>
#define FUN(n, d) 	{#n, n, METH_VARARGS, d }

#define RETURN_PTR(x)  		return PyCapsule_New(x, NULL, NULL)
#define RETURN_TPTR(t, x)	return PyCapsule_New(x, #t, NULL)
#define RETURN_NONE			Py_RETURN_NONE
#define RETURN_BOOL(x)		{ if(x) Py_RETURN_TRUE; else Py_RETURN_TRUE; }
#define RETURN_INT(x)		return Py_BuildValue("i", x)
#define RETURN_UINT(x)		return Py_BuildValue("I", x)
#define RETURN_LONG(x)		return Py_BuildValue("L", x)
#define RETURN_ULONG(x)		return Py_BuildValue("K", x)
#define RETURN_FLOAT(x)		return Py_BuildValue("f", x)
#define RETURN_DOUBLE(x)	return Py_BuildValue("d", x)
#define RETURN_STR(x)		return Py_BuildValue("s", x)

#define PTR(t, x)		((t *)PyCapsule_GetPointer(x, NULL))
#define TPTR(t, x)		((t *)PyCapsule_GetPointer(x, #t))

static PyObject *
new_board(PyObject *self, PyObject *args) {
	const char *name;
	PyObject *omem;
	if(!PyArg_ParseTuple(args, "sO", &name, &omem))
		return NULL;
	csim_memory_t *mem;
	if(omem == Py_None)
		mem = NULL;
	else
		mem = TPTR(csim_memory_t, omem);
	csim_board_t *board = csim_new_board(name, mem);
	board->level = CSIM_DEBUG;
	RETURN_TPTR(csim_board_t, board);
}

static PyObject *
delete_board(PyObject *self, PyObject *args) {
	PyObject *board;
	if(!PyArg_ParseTuple(args, "O", &board))
		return NULL;
	csim_delete_board(TPTR(csim_board_t, board));
	RETURN_NONE;
}

static PyObject *
run(PyObject *self, PyObject *args) {
	PyObject *board;
	uint64_t time;
	if(!PyArg_ParseTuple(args, "OK", &board, &time))
		return NULL;
	csim_run(TPTR(csim_board_t, board), time);
	RETURN_NONE;
}

static PyMethodDef csim_methods[] = {
	FUN(new_board, "(name, memory) Create a new board."
		"memory may be None. Return the board."),
	FUN(delete_board, "(board) Delete the given board"),
	FUN(run, "(board, time) Run the board during time cycles"),
	{NULL, NULL, 0, NULL}
};

static struct PyModuleDef csim_module = {
    PyModuleDef_HEAD_INIT,
    "csim",
    "Compmonent Simulation Library",
    -1,
    csim_methods
};

PyMODINIT_FUNC
PyInit_csim(void) {
    return PyModule_Create(&csim_module);
}
