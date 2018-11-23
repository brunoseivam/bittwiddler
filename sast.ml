(* Semantically-checked Abstract Syntax Tree *)

open Ast

type sexpr = type_ * sx

and sx =
    SLInt of int
  | SLFloat of float
  | SLString of string
  | SLArray of sexpr list
  | SId of string
  | SEType of ptype
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SMatch of sexpr * (sexpr * sblock_item list) list
  | SCond of (sexpr option * sblock_item list) list
  | SFor of string list * sexpr * sblock_item list
  | SWhile of sexpr * sblock_item list
  | SCall of string * sexpr list
  | STCall of ptype * expr list

and sblock_item =
    SLVar of svar (* local variable declaration *)
  | SExpr of sexpr
  | SReturn of sexpr

and svar =
    SVar of bool * string * type_ option * expr option

type sparam =
    SParam of string * type_

type sprogram_decl =
    SFunc of string * type_ * sparam list * sblock_item list
(*| STemplate of string * sparam list * stemplate_item list *)
  | SGVar of svar (* global variable *)


type sprogram = SProgram of sprogram_decl list * sblock_item list

(* Pretty-printing *)

let string_of_sprogram = function _ -> ""

(*let string_of_sprogram = function
    SProgram(pdecls, block) -> string_of_program (Program(pdecls, block))*)
