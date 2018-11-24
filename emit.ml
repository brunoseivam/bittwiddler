open Str

(* Ad-hoc parsing of the string passed to 'emit'.
 * I tried doing this with ocamllex / ocamlyacc
 * but it seemed to be too complicated for this case.
 *)

type token =
    STR of string
  | VAR of string list

let parse_emit_fmt s =
    let id = "[a-z_][a-zA-Z0-9_]*" in
    let var = Str.regexp ("{" ^ id ^ "\\(\\." ^ id ^ "\\)*}") in

    let rec tokens = function
        [] -> []
      | Text(s)::tl ->
              STR(s)::(tokens tl)
      | Delim(s)::tl ->
              VAR(Str.split (Str.regexp "\\.\\|{\\|}") s)::(tokens tl)
    in

    tokens (Str.full_split var s)
