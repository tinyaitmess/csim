/* Generated by gliss-csim ($(date)) copyright (c) 2024 IRIT - UPS */

#include <stdio.h>
#include <stdlib.h>
#include <csim.h>
#include "csim-rt.h"

#define ____COMP_NUM		(inst->number)
$(foreach registers)
#	define _$(NAME)	__inst->$(name)
$(end)
$(foreach ports)
#	define $(name)_BASE	$(base)
#	define _$(name)	__inst->$(name)
#	define $(name)_PORT	&ports[$(name)_BASE]
	$(if multiple)
#		define $(name)_SET(i, x)	\
			{ \
				if(_$(name)[i] != x) { \
					_$(name)[i] = x; \
					csim_send_digital(inst, $(name)_PORT + i, _$(name)[i]); \
				} \
			}
	$(else)
#		define $(name)_SET(x)	\
			{ \
				if(x != _$(name)) { \ 
					_$(name) = x; \
					csim_send_digital(inst, $(name)_PORT, _$(name)); \
				} \
			}
	$(end)
$(end)

#define __now inst->board->date



/**
 * Instance definition.
 */
typedef struct  $(comp)_inst_t {
	$(if io_comp)
    csim_iocomp_t inst;
	$(else)
	csim_inst_t inst;
	$(end)
    $(foreach registers)
		$(if multiple)
			$(type) $(name)[$(size)];
		$(else)
			$(type) $(name);
		$(end)
    $(end)
    $(foreach ports)
		$(if multiple)
			$(ctype) $(name)[$(count)];
		$(else)
			$(ctype) $(name);
		$(end)
	$(end);
}  $(comp)_inst_t;


/* pre-definitiion of port update functions */
$(foreach ports)
	static void on_update_$(name)(csim_inst_t *inst);
$(end)

/* pre-definitiion of event update functions */
$(foreach events)
	static void on_update_$(name)(csim_inst_t *inst);
$(end)

/* pre-definitiion of event trigger functions */
$(foreach events)
	static void on_trigger_$(name)(csim_evt_t *evt);
$(end)

static void on_update_all(csim_inst_t *inst) {
	$(foreach ports)
		on_update_$(name)(inst);
	$(end)	
	$(foreach events)
		on_update_$(name)(inst);
	$(end)	
}


/**
 * Reset the instance.
 * @param inst	Instance to reset.
 */
static void  $(comp)_reset(csim_inst_t *inst) {
    $(comp)_inst_t * $(comp)_inst = ( $(comp)_inst_t *)inst;
	$(foreach registers)
	$(comp)_inst -> $(name) = $(init);
	$(end)
}

/**
 * Construct an instance.
 * @param inst	Instance to construct.
 */
static void $(comp)_construct(csim_inst_t *inst, csim_confs_t confs) {
    $(comp)_inst_t * $(comp)_inst = ( $(comp)_inst_t *)inst;
    $(comp)_reset(inst);
}

/**
 * Destruct the instance.
 * @param inst	Instance to destruct.
 */
static void $(comp)_destruct(csim_inst_t *inst) {
}


$(foreach registers)
$(if !intern)
/* $(name) register functions */

static void name_$(name)(csim_inst_t *inst, int num, char *__buffer, int size) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
	$(label)
}


static void display_$(name)(csim_inst_t *inst, int num, char *__buffer, int size) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
	$(display);
}

static csim_word_t read_$(name)(csim_inst_t *inst, int num) {
	$(if is_read_only)
		csim_log(inst->board, CSIM_WARN, "read of ${comp}.${name} that is read-only");
	$(else)
		$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
		$(ifdef on_read)
			$(on_read);
		$(else)
			$(if multiple)
				return __inst->$(name)[num];
			$(else)
				return __inst->$(name);
			$(end)
		$(end)
	$(end)
}
 
static csim_word_t get_$(name)(csim_inst_t *inst, int num) {
	$(if is_write_only)
		csim_log(inst->board, CSIM_WARN, "write of ${comp}.${name} that is write-only");
	$(else)
		$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
		$(if multiple)
			return __inst->$(name)[num];
		$(else)
			return __inst->$(name);
		$(end)
	$(end)
}
 
static void set_$(name)(csim_inst_t *inst, int num, csim_word_t val) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
	$(if multiple)
		if(__inst->$(name)[num] != val) {
			__inst->$(name)[num] = val;
			on_update_all(inst);
		}
	$(else)
		if(__inst->$(name) != val) {
			__inst->$(name) = val;
			on_update_all(inst);
		}
	$(end)	
}

static void write_$(name)(csim_inst_t *inst, int num, csim_word_t val) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
	set_$(name)(inst, num, val);

	$(ifdef on_write)
		$(on_write);
		on_update_all(inst);
	$(end)	
}

$(end)
$(end)


/**
 * Array of registers.
 */
static csim_reg_t regs[] = {
	$(foreach registers)
	{
		"$(name)",		// name
		$(offset),		// offset
		$(size),		// size
		$(count),		// count
		$(stride),		// stride
		0,				// flags
		$(ctype),		// signal type
		$(if !intern)
		name_$(name),
		display_$(name),
		read_$(name),
		write_$(name),
		get_$(name),
		set_$(name)
		$(end)
	},
	$(end)
};


/* predeclaration of port-functions */
$(foreach ports)
static void on_input_$(name)(csim_port_inst_t *port, csim_value_type_t type, csim_value_t val);
$(end)


/**
 * Array of ports.
 */
static csim_port_t ports[] = {
	$(foreach ports)
		$(if multiple)
			$(foreach indexes)
				{
					"$(name)$(index)",
					$(type),
					on_input_$(name)
				},
			$(end)
		$(else)
			{
				"$(name)",
				$(type),
				on_input_$(name)
			},
		$(end)
	$(end)
};


$(foreach ports)
/* $(name) port functions */

static void on_input_$(name)(csim_port_inst_t *port, csim_value_type_t type, csim_value_t val) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)port->inst;
	$(ctype) $(name) = __inst->$(name);
	int ____INDEX = port->port - (ports + $(name)_BASE);
	if (type == CSIM_DIGITAL)
		$(name) = val.digital;
	if (type == CSIM_ANALOG)
		$(name) = val.analog;
	if (type == CSIM_CLOCK)
		$(name) = val.clock;
	if (type == CSIM_SERIAL)
		$(name) = val.serial;
	
	
	
$(on_input)
}

static void on_update_$(name)(csim_inst_t *inst) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;
$(on_update)
}

$(end)

$(foreach events)
/*$(name) event functions */

static void on_update_$(name)(csim_inst_t *inst) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)inst;

	$(on_update)
}

static void on_trigger_$(name)(csim_evt_t *evt) {
	$(comp)_inst_t *__inst = ($(comp)_inst_t *)evt->inst;
	csim_inst_t *inst = evt->inst;

	$(on_trigger)
}

$(end)

$(if io_comp)

static int $(comp)_display(char *buf, csim_iocomp_inst_t *inst) {
	char display[] = " ";
	// By default shows nothing, can be modified by changing the display var.
	return sprintf(buf,display);
}

static void $(comp)_on_key(char key, csim_iocomp_inst_t *inst) {
}


csim_iocomp_t  $(comp)_component = {
	{
    "$(comp)",
    CSIM_IO,
    0,					// version
    regs,				// registers
    $(register_count) ,	// register count
    ports,				// ports
    $(port_count),		// port count
    sizeof($(comp)_inst_t),
    $(comp)_construct,
    $(comp)_destruct,
    $(comp)_reset
	},
	$(comp)_display,
	$(comp)_on_key
};

$(else)
/**
 * $(comp) description structure.
 */
csim_component_t  $(comp)_component = {
    "$(comp)",
    CSIM_SIMPLE,
    0,					// version
    regs,				// registers
    $(register_count) ,	// register count
    ports,				// ports
    $(port_count),		// port count
    sizeof($(comp)_inst_t),
    $(comp)_construct,
    $(comp)_destruct,
    $(comp)_reset
};
$(end)