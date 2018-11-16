(* Semantically-checked Abstract Syntax Tree *)

open Ast

(* For now, a semantically checked program is exactly like a program *)

type sprogram = SProgram of program_decl list * block_item list

(* Pretty-printing *)

let string_of_sprogram = function
    SProgram(pdecls, block) -> string_of_program (Program(pdecls, block))
