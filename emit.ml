open Str

(* Ad-hoc parsing of the string passed to 'emit' *)

type token =
    STR of string
  | VAR of string

let parse_emit_fmt s =
    let id = Str.regexp "{[a-z_][a-zA-Z0-9_]*}" in

    let rec tokens = function
        [] -> []
      | Text(s)::tl ->
              STR(s)::(tokens tl)
      | Delim(s)::tl ->
              VAR(Str.global_replace (Str.regexp "{\\|}") "" s)::(tokens tl)
    in

    tokens (Str.full_split id s)
