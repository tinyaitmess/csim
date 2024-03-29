#include "arm_core.h"

typedef struct {
	csim_inst_t inst;
} arm_core_inst_t;

static csim_reg_t regs[] = {};
static csim_port_t ports[] = {};

static void construct(csim_inst_t *inst, csim_confs_t confs) {
}

void destruct(csim_inst_t *inst) {
}

void reset(csim_inst_t *inst) {
}

void step(csim_core_inst_t *comp) {
	
}

csim_core_t arm_component = {
	{
		"arm",
		CSIM_CORE,
		1,		// version
		regs, 0,
		ports, 0,
		sizeof(arm_core_inst_t),
		construct,
		destruct,
		reset
	},
	0,	// clock?
	step
};
