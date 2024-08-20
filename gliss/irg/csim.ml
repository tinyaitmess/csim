
(** CSIM extension module. *)

open Printf
open Irg

(* Declare a specification.
	@param spec	Declared specification. *)
let declare spec =
	add_symbol (name_of spec) spec

(** Prepare parsing for CSIM component parsing i.e.
	declare ___COMP_NUM. *)
let prepare_parsing _ =
	declare (Irg.PARAM ("__COMP_NUM", TYPE_EXPR(CARD(32))))

(** Clean the symbols passed as parameter.
	@param syms		Clean symbols used by registers or ports. *)
let clean_symbols _ =
	List.iter rm_symbol [
		"__INDEX";
		"__VALUE"
	]

(** Prepare other type of specification than register or port. *)
let prepare_other _ =
	clean_symbols ()

(** Prepare a register with given typeand count.
	@param count	Register count.
	@param typ		Type of register.*)
let prepare_reg count typ =
	clean_symbols ();
	declare (PARAM ("__VALUE", TYPE_EXPR typ));
	if count > 1 then
		declare (PARAM ("__INDEX", TYPE_EXPR (CARD(32))))

(** Prepare a poer with given typeand count.
	@param count	Port count.
	@param typ		Type of port.*)
let prepare_port count typ =
	clean_symbols ();
	declare (PARAM ("__VALUE", TYPE_EXPR typ));
	if count > 1 then
		declare (PARAM ("__INDEX", TYPE_EXPR (CARD(32))))
	

