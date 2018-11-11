(* Semantically-checked Abstract Syntax Tree *)

open Ast

(* For now, a semantically checked program is exactly like a program *)

type sprogram = SProgram of pdecl list * parse

(* Pretty-printing *)

let string_of_sprogram = function
    SProgram(pdecls, parse) -> string_of_program (Program(pdecls, parse))
