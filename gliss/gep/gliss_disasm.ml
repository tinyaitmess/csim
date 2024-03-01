(*
 * GLISS2 -- disassembly gnerator
 * Copyright (c) 2008, IRIT - UPS <casse@irit.fr>
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
 *)

open Disasm
open Irg
open Printf
exception CommandError of string

(* library path *)
let paths = [
	Config.install_dir ^ "/lib/gliss/lib";
	Config.source_dir ^ "/lib";
	Sys.getcwd ()]


(* argument list *)
let out = ref "disasm.c"
let command = ref false
let options = [
	("-o", Arg.Set_string out, "output file");
	("-c", Arg.Set command, "generate also the command")
] @ Stot.opts


(** Perform the disassembling of the given instruction.
	@param inst		Instruction to get syntax from.
	@param out		Output to use. *)
let disassemble inst out info =
	info.Toc.out <- out;
	Toc.set_inst info inst;

	(* get syntax *)
	let syntax =
		try
			match Iter.get_attr inst "syntax" with
			  Iter.STAT _ -> raise (Toc.Error "syntax must be an expression")
			| Iter.EXPR e -> e
		with Not_found -> raise (Toc.Error "no attribute") in

	(* disassemble *)
	let params = Iter.get_params inst in
	Irg.param_stack params;
	let stats = Toc.prepare_stat info (Disasm.gen info inst syntax) in
	Toc.declare_temps info;
	Toc.gen_stat info stats;
	Toc.cleanup_temps info;
	Irg.param_unstack params


let _ =
	App.run
		options
		"SYNTAX: gep [options] NML_FILE\n\tGenerate code for a simulator"
		(fun info ->
			Irg.add_symbol "__buffer" (Irg.VAR ("__buffer", 1, Irg.NO_TYPE, []));

		try

				(* transform switches *)
				Stot.transform_aux "syntax";

				(* generate disassemble source *)
				let maker = App.maker () in
				maker.App.get_instruction <- (fun inst dict ->
					("disassemble", Templater.TEXT (fun out -> disassemble inst out info)) ::
					dict);
				let dict =
					("declare_switch", Templater.TEXT (fun out -> info.Toc.out <- out; Stot.declare info)) ::
					(App.make_env info maker) in
				if not !App.quiet then (Printf.printf "creating \"%s\"\n" !out; flush stdout);
				Templater.generate dict "disasm.c" !out;

				(* generate the command *)
				if !command then
					begin
						try
							let path = App.find_lib "disasm/disasm.c" paths in
							App.makedir "disasm";
							App.replace_gliss info
								(path ^ "/" ^ "disasm/disasm.c")
								("disasm/" ^ info.Toc.proc ^ "-disasm.c" );
							Templater.generate_path
								[ ("proc", Templater.TEXT (fun out -> output_string out info.Toc.proc)) ]
								(path ^ "/disasm/Makefile")
								"disasm/Makefile"
						with Not_found ->
							raise (CommandError  "no template to make disasm program")
					end
		with
		| CommandError msg -> raise (Irg.Error (Irg.asis msg))
		| Toc.PreError f -> raise (Irg.Error f)
	)
