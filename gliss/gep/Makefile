# $Id: Makefile,v 1.13 2009/09/15 07:50:48 casse Exp $
include ../Makefile.head

OCAML_INC=../irg ../optirg
ifndef NDEBUG
OCAML_FLAGS+=-g
OCAML_LDFLAGS+=-g
endif

# libgep build
LIBGEP_SOURCES=\
	bitmask.ml \
	opti.ml \
	toc.ml \
	templater.mll \
	app.ml \
	profile.ml \
	decode_arg.ml \
	bits.ml \
	stot.ml \
	pqmc.ml
$(eval $(call ocaml_byte_code_lib,libgep,$(LIBGEP_SOURCES)))

# GEP build
GEP_SOURCES = \
	fetch.ml \
	decode.ml \
	gep.ml
gep_LIBS = str unix  ../irg/irg ../optirg/liboptirg ../gep/libgep
$(eval $(call ocaml_prog,gep,$(GEP_SOURCES)))

gep: ../irg/irg.cma ../gep/libgep.cma

# gliss-disasm build
DISASM_SOURCES = \
	disasm.ml \
	gliss_disasm.ml

gliss-disasm_LIBS = str unix ../irg/irg ../optirg/liboptirg libgep
$(eval $(call ocaml_prog,gliss-disasm,$(DISASM_SOURCES)))
gliss-disasm: ../irg/irg.cma  libgep.cma


# gliss-attr build
ATTR_SOURCES = \
	gliss_attr.ml

gliss-attr_LIBS = str unix ../irg/irg ../optirg/liboptirg libgep
$(eval $(call ocaml_prog,gliss-attr,$(ATTR_SOURCES)))
gliss-attr: ../irg/irg.cma libgep.cma


# gliss-attr build
USED_REGS_SOURCES = \
	gliss_used_regs.ml


# gliss-csim build
CSIM_SOURCES = \
	disasm.ml \
    gliss_csim.ml

gliss-csim_LIBS = str unix ../irg/irg ../optirg/liboptirg libgep
$(eval $(call ocaml_prog,gliss-csim,$(CSIM_SOURCES)))
gliss-csim: ../irg/irg.cma libgep.cma

include ../Makefile.tail
