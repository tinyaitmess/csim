#include "led.h"

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

