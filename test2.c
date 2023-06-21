/*
 * Component simulator test program
 * Copyright (c) 2019, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of GLISS2.
 *
 * GLISS2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <sys/select.h>

#include "csim.h"
#include "mem.h"
#include "yaml.h"
#include <arm/api.h>
#include <arm/loader.h>

#define INST_SLICE	10

/****** LED ******/

typedef struct led_inst_t {
	csim_inst_t inst;
	int state;
} led_inst_t;

void led_reset(csim_inst_t *inst) {
	led_inst_t *i = (led_inst_t *)inst;
	i->state = 0;
}

void led_construct(csim_inst_t *inst) {
	led_reset(inst);
}

void led_destruct(csim_inst_t *i) { }

void led_update(csim_port_inst_t *inst, csim_value_type_t type, csim_value_t val) {
	led_inst_t *i = (led_inst_t *)inst;
	i->state = val.digital;
}

csim_port_t led_ports[] = {
	{ "output", CSIM_DIGITAL, led_update }
};

void led_write(csim_inst_t *inst, int n, csim_word_t v) {
	led_inst_t *i = (led_inst_t *)inst;
	i->state = v;
	inst->board->log(inst->board, CSIM_INFO, "led_write(%d, %d)", n, v);
}

csim_word_t led_read(csim_inst_t *inst, int n) {
	led_inst_t *i = (led_inst_t *)inst;
	inst->board->log(inst->board, CSIM_INFO, "led_read(%d)", n);
	return i->state;
}

csim_reg_t led_regs[] = {
	{ "R", 0, 4, 1, 1, 0, CSIM_INT, NULL, NULL, led_read, led_write, NULL, NULL }
};

csim_component_t led_component = {
	"led",
	CSIM_SIMPLE,
	1,
	led_regs,
	1,
	led_ports,
	0,
	sizeof(led_inst_t),
	led_construct,
	led_destruct,
	led_reset,
	NULL
};

int led_state(csim_inst_t *inst) {
	led_inst_t *i = (led_inst_t *)inst;
	return i->state;
}


/****** Button ******/

typedef struct button_inst_t {
	csim_inst_t inst;
	int pushed;
} button_inst_t;

void button_reset(csim_inst_t *inst) {
	button_inst_t *i = (button_inst_t *)inst;
	i->pushed = 0;
}

void button_construct(csim_inst_t *inst) {
	button_reset(inst);
}

void button_destruct(csim_inst_t *c) {
}

void button_write(csim_inst_t *inst, int n, csim_word_t v) {
	inst->board->log(inst->board, CSIM_INFO, "button_write(%d, %d)", n, v);
}

csim_word_t button_read(csim_inst_t *inst, int n) {
	button_inst_t *i = (button_inst_t *)inst;
	inst->board->log(inst->board, CSIM_INFO, "button_read(%d)", n);
	return i->pushed;
}

csim_reg_t button_regs[] = {
	{ "R", 0, 4, 1, 1, 0, CSIM_INT, NULL, NULL, button_read, button_write, NULL, NULL }
};

void button_update(csim_port_inst_t *port, csim_value_type_t type, csim_value_t val) {
}

csim_port_t button_ports[] = {
	{ "output", CSIM_DIGITAL, button_update }
};


csim_component_t button_component = {
	"button",
	CSIM_SIMPLE,
	1,
	button_regs,
	1,
	button_ports,
	1,
	sizeof(button_inst_t),
	button_construct,
	button_destruct,
	button_reset
};

int button_get(csim_inst_t *inst) {
	button_inst_t *i = (button_inst_t *)inst;
	return i->pushed;
}

void button_set(csim_inst_t *inst, int pushed) {
	button_inst_t *i = (button_inst_t *)inst;
	if(pushed != i->pushed) {
		i->pushed = pushed;
		csim_send_digital(inst, &button_ports[0], pushed);
	}
}


/****** Board ******/

csim_component_t *comps[32] = {
	&led_component,
	&button_component,
	NULL
};

typedef struct {
	csim_board_t *board;
	csim_inst_t *leds[16];
	int led_cnt;
	struct {
		csim_inst_t *but;
		char key;
	} buts[16];
	int but_cnt;
} board_t;

typedef struct {
	enum {
		TOP,
		IN_COMPS,
		IN_COMP
	} state;
	board_t *board;
	csim_memory_t *mem;
	const char *name, *type;
	char key;
	arm_address_t base;
} loader_t;

int VERBOSE = 0;

/**
 * Free resources used by the board..
 */
void finish_board(board_t *board) {
	csim_delete_board(board->board);
}

/**
 * Initialize a board.
 * @return		Empty board.
 */
void init_board(board_t *board) {
	board->led_cnt = 0;
	board->but_cnt = 0;
	board->board = NULL;
}

/**
 * Add a LED to the board.
 */
void add_led(board_t *board, csim_inst_t *inst) {
	board->leds[board->led_cnt++] = inst;
}

/**
 * Add a button to the board.
 */
void add_button(board_t *board, csim_inst_t *inst, char key) {
	board->buts[board->but_cnt].but = inst;
	board->buts[board->but_cnt++].key = key;
}

///
static yaml_next_t on_key(const char *key, const char *val, void *data) {
	loader_t *loader = (loader_t *)data;
	switch(loader->state) {

	case TOP:
		if(strcmp(key, "name") == 0) {
			loader->name = strdup(val);
			return YAML_DONE;
		}
		else if(strcmp(key, "components") == 0) {
			loader->board->board = csim_new_board(loader->name, loader->mem);
			loader->board->board->level = CSIM_ERROR;
			loader->state = IN_COMPS;
			return YAML_MAP;
		}
		break;

	case IN_COMPS:
		loader->name = strdup(key);
		loader->state = IN_COMP;
		return YAML_MAP;

	case IN_COMP:
		if(strcmp(key, "type") == 0) {
			loader->type = strdup(val);
			return YAML_DONE;
		}
		else if(strcmp(key, "base") == 0) {
			sscanf(val, "%x", &loader->base);
			return YAML_DONE;
		}
		else if(strcmp(key, "key") == 0) {
			sscanf(val, "%c", &loader->key);
			return YAML_DONE;
		}
		break;
	}
	return YAML_ERROR;
}

///
static void on_end(void *data) {
	loader_t *loader = (loader_t *)data;

	switch(loader->state) {

	case TOP:
		break;

	case IN_COMP: {

			/* find the component */
			csim_component_t *type = NULL;

			/* build the component */
			for(int i = 0; comps[i] != NULL; i++)
				if(strcmp(loader->type, comps[i]->name) == 0) {
					type = comps[i];
					break;
				}
			if(type == NULL) {
				fprintf(stderr, "ERROR: component type %s does not exist!\n", loader->type);
				exit(1);
			}

			/* buiild the component */
			csim_inst_t *inst = csim_new_component(loader->board->board, type, loader->name, loader->base);
			if(type == comps[0])
				add_led(loader->board, inst);
			else if(type == comps[1])
				add_button(loader->board, inst, loader->key);
			loader->state = IN_COMPS;
		}
		break;

	case IN_COMPS:
		loader->state = TOP;
		break;
	}
}

/**
 * Load the content of a board from given file.
 *
 * In case of error, display it and stop the program.
 * @param board		Board to initialize.
 * @param path		Path to read board from.
 * @param mem		Memory to use for I/O registers.
 */
void load_board(board_t *board, const char *path, csim_memory_t *mem) {
	init_board(board);
	loader_t loader = { TOP, board, mem, "anonymous", NULL, '\0', 0 };
	yaml_handler_t handler;
	yaml_init_handler(&handler);
	handler.on_key = on_key;
	handler.on_end = on_end;
	int num = yaml_parse(&handler, path, &loader);
	if(num != 0) {
		fprintf(stderr, "ERROR:%s:%d: syntax error.\n", path, num);
		exit(1);
	}
}


/****** Simulator ******/
void reset_console() {
	struct termios t;
	tcgetattr(0, &t);
	t.c_lflag |= ECHO | ICANON;
	tcsetattr(0, TCSANOW, &t);
	printf("\033[?25h\n");
	fflush(stdout);
}

void on_control_c(int x) {
	reset_console();
	exit(0);
}

void init_console() {
	struct termios t;
	tcgetattr(0, &t);
	t.c_lflag &= ~(ECHO | ICANON);
	tcsetattr(0, TCSANOW, &t);

	atexit(reset_console);
	signal(SIGINT, on_control_c);
	printf("\033[?25l");
}

//csim_inst_t *led, *button;
char buf[256] = "";
arm_sim_t *sim;


/**
 * Print the state.
 */
void print_state(board_t *board, int clear) {
	int buf_size;

	if(clear) {
		for(buf_size = 0; buf[buf_size] != '\0'; buf_size++)
			buf[buf_size] = '\b';
		fputs(buf, stdout);
	}

	char *p = buf;
	for(int i = 0; i < board->led_cnt; i++)
		p += sprintf(p, "[%c] ", led_state(board->leds[i]) ? '*' : ' ');
	for(int i = 0; i < board->but_cnt; i++) {
		int pushed = button_get(board->buts[i].but);
		p += sprintf(p, "%c%c%c ",
				(pushed ? ')' : '('),
				board->buts[i].key,
				(pushed ? '(' : ')')
			);
	}
	p += sprintf(p, "%08x ", arm_next_addr(sim));
	arm_inst_t *inst = arm_next_inst(sim);
	arm_disasm(p, inst);
	arm_free_inst(inst);

	if(clear) {
		int size = strlen(buf);
		while(size < buf_size) {
			buf[size] = ' ';
			size++;
		}
		buf[size] = '\0';
	}

	fputs(buf, stdout);
}

/**
 * Display the options.
 */
void print_help() {
	fprintf(stderr, "SYNTAX: test2 FICHIER.elf\n");
	fprintf(stderr, "\t-h, -help: displays help message.\n");
	fprintf(stderr, "\t-v: verbose mode.\n");
}

/**
 * Application entry.
 */
int main(int argc, const char *argv[]) {
	const char *exec = NULL;

	/* parse arguments */
	for(int i = 1; i < argc; i++) {
		if(argv[i][0] != '-') {
			if(exec != NULL) {
				print_help();
				fprintf(stderr, "ERROR: several executable provided: %s\n", argv[1]);
				exit(1);
			}
			else
				exec = argv[i];
		}
		else if(strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0) {
			print_help();
			exit(0);
		}
		else if(strcmp(argv[i], "-v") == 0)
			VERBOSE = 1;
		else {
			print_help();
			fprintf(stderr, "ERROR: unknown option: %s\n", argv[i]);
			exit(1);
		}
	}

	/* check arguments */
	if(exec == NULL) {
		print_help();
		fprintf(stderr, "ERROR: executable needed!\n");
		exit(1);
	}

	/* load the executable */
	arm_loader_t *loader = arm_loader_open(exec);
	if(loader == NULL) {
		fprintf(stderr, "ERROR: cannot load %s\n", exec);
		exit(1);
	}

	/* look for _start and _exit */
	arm_address_t _start, _exit;
	int start_done = 0, exit_done = 0;
	for(int i = 0; i < arm_loader_count_syms(loader); i++) {
		arm_loader_sym_t sym;
		arm_loader_sym(loader, i, &sym);
		if(strcmp(sym.name, "_start") == 0) {
			_start = sym.value;
			start_done = 1;
		}
		else if(strcmp(sym.name, "_exit") == 0) {
			_exit = sym.value;
			exit_done = 1;
		}
		if(start_done && exit_done)
			break;
	}
	if(!start_done) {
		fprintf(stderr, "ERROR: no _start symbol!\n");
		exit(1);
	}

	/* build the ARMv5 simulator */
	arm_platform_t *pf = arm_new_platform();
	arm_load(pf, loader);
	arm_state_t *state = arm_new_state(pf);
	sim = arm_new_sim(state, _start, _exit);

	/* build the board */
	board_t board;
	int l = strlen(argv[1]);
	char path[256];
	if(strcmp(".elf", argv[1] + l - 4) == 0) {
		strncpy(path, argv[1], l-4);
		path[l-4] = '\0';
	}
	else
		strcpy(path, argv[1]);
	strcat(path, ".yaml");
	if(access(path, R_OK) == 0)
		load_board(&board, path, arm_get_memory(pf, ARM_MAIN_MEMORY));
	else {
		init_board(&board);
		board.board = csim_new_board("default", arm_get_memory(pf, ARM_MAIN_MEMORY));
		add_led(&board, csim_new_component(board.board, &led_component, "led", 0xA0000000));
		add_button(&board, csim_new_component(board.board, &button_component, "button", 0xB0000000), 'a');
	}

	// initialize input
	fd_set set;
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	init_console();

	// perform I/O
	print_state(&board, 0);
	while(1) {

		// update display
		print_state(&board, 1);

		// get the key
		FD_ZERO(&set);
		FD_SET(0, &set);
		int n = select(1, &set, NULL, NULL, &tv);
		if(n != 0) {
			char key;
			read(0, &key, 1);
			for(int i = 0; i < board.but_cnt; i++)
				if(key == board.buts[i].key)
					button_set(board.buts[i].but, !button_get(board.buts[i].but));
		}

		// simulate the code
		for(int i = 0; i < INST_SLICE && !arm_is_sim_ended(sim); i++)
			arm_step(sim);

		// run the board
		csim_run(board.board, 1);
	}
	return 0;
}

