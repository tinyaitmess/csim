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
#include <linux/input.h>

#include "csim.h"
#include "mem.h"
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
	"LED",
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
	"Button",
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

#define LED_BASE	0xA0000000
#define BUT_BASE	0xB0000000

csim_inst_t *led, *button;
char buf[256];
arm_sim_t *sim;

void clear_state() {
	for(int i = 0; buf[i] != '\0'; i++)
		buf[i] = '\b';
	fputs(buf, stdout);
}

void print_state() {
	sprintf(buf,
		"[%c] %s %08x ",
		(led_state(led) ? '*' : ' '),
		(button_get(button) ? ")a(" : "[A]"),
		arm_next_addr(sim));
	arm_inst_t *inst = arm_next_inst(sim);
	arm_disasm(buf + 17, inst);
	arm_free_inst(inst);
	fputs(buf, stdout);
}

int main(int argc, const char *argv[]) {

	/* load the executable */
	if(argc != 2) {
		fprintf(stderr, "SYNTAX: test2 FICHIER.elf\n");
		exit(1);
	}
	arm_loader_t *loader = arm_loader_open(argv[1]);
	if(loader == NULL) {
		fprintf(stderr, "ERROR: cannot load %s\n", argv[1]);
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
	csim_memory_t *mem = arm_get_memory(pf, ARM_MAIN_MEMORY);
	csim_board_t *board = csim_new_board("my-board", mem);
	board->level = CSIM_ERROR;

	led = csim_new_component(board, &led_component, "led", LED_BASE);
	button = csim_new_component(board, &button_component, "button", BUT_BASE);

	// initialize input
	fd_set set;
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	init_console();

	// perform I/O
	print_state();
	while(1) {

		// update display
		clear_state();
		print_state();

		// get the key
		FD_ZERO(&set);
		FD_SET(0, &set);
		int n = select(1, &set, NULL, NULL, &tv);
		if(n != 0) {
			char key;
			read(0, &key, 1);
			if(key == 'a')
				button_set(button, !button_get(button));
		}

		// simulate the code
		for(int i = 0; i < INST_SLICE && !arm_is_sim_ended(sim); i++)
			arm_step(sim);

		// run the board
		csim_run(board, 1);
	}
	return 0;
}

