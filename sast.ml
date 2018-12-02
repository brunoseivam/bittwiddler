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
  | STCall of ptype * sexpr list

and sblock_item =
    SLVar of svar (* local variable declaration *)
  | SExpr of sexpr
  | SReturn of sexpr

and svar = bool * string * type_ * sexpr option

type sparam = string * type_

type stemplate_item =
    SField of svar
  | STField of sexpr * sexpr option * sexpr option
  | STExpr of sexpr

type sfunc = string * type_ * sparam list * sblock_item list
type stempl = string * sparam list * stemplate_item list

type sprogram_decl =
    SFunc of sfunc
  | STemplate of stempl
  | SGVar of svar (* global variable *)

type sprogram = sprogram_decl list

(* Pretty-printing *)

let rec string_of_stblock_item = function
    SField(v) -> (string_of_svar v) ^ ";"
  | STField(e1, e2, e3) ->
        let e1 = (string_of_sexpr e1) in
        let e2 = (match e2 with Some e -> (string_of_sexpr e) | None -> "") in
        let e3 = (match e3 with Some e -> (string_of_sexpr e) | None -> "") in
        "STField(id=" ^ e1 ^ ", type=" ^ e2 ^ ", value=" ^ e3 ^ ");"
  | STExpr(e) -> (string_of_sexpr e) ^ ";"

and string_of_stblock b =
    "{\n" ^ (String.concat "\n" (List.map string_of_stblock_item b))

and string_of_sblock_item = function
    SLVar(v) -> (string_of_svar v) ^ ";"
  | SExpr(e) -> (string_of_sexpr e) ^ ";"
  | SReturn(e) -> "SReturn(" ^ (string_of_sexpr e) ^ ");"

and string_of_sblock b =
    "{\n" ^ (String.concat "\n" (List.map string_of_sblock_item b)) ^ "}"

and string_of_sarm arm =
    let (e, block) = arm in
    (string_of_sexpr e) ^ " -> " ^ (string_of_sblock block)

and string_of_scond cond =
    match cond with
    (Some e, b) -> "SElseIf(" ^ (string_of_sexpr e) ^ ") "
                   ^ (string_of_sblock b)
  | (None, b) -> "SElse " ^ (string_of_sblock b)

and string_of_sx = function
    SLInt i -> string_of_int i
  | SLFloat f -> string_of_float f
  | SLString s -> "\"" ^ s ^ "\""
  | SLArray lx -> "[" ^ (String.concat "," (List.map string_of_sexpr lx)) ^ "]"
  | SId id -> id
  | SEType t -> "SEType(" ^ (string_of_ptype t) ^ ")"
  | SBinop(e1,op,e2) ->
          "(" ^ (string_of_op op)
          ^ " " ^ (string_of_sexpr e1)
          ^ " " ^ (string_of_sexpr e2) ^ ")"
  | SUnop(uop,e) ->
          "(" ^ (string_of_uop uop) ^ " " ^ (string_of_sexpr e) ^ ")"
  | SMatch(e,arms) ->
          "SMatch(" ^ (string_of_sexpr e) ^ ") {\n"
          ^ (String.concat "\n" (List.map (string_of_sarm) arms)) ^ "\n}"
  | SCond(conds) -> (match conds with
        (e,b)::tl -> (match e with
            Some e -> "SIf(" ^ (string_of_sexpr e) ^ ")"
                      ^ (string_of_sblock b) ^ "\n"
                      ^ (String.concat "\n" (List.map (string_of_scond) tl))
          | None -> raise (Failure "malformed conditional"))
      | _ -> raise (Failure "malformed conditional")
  )
  | SFor(ids, e, b) ->
          "SFor( (" ^ (String.concat "," ids) ^ ") in " ^ (string_of_sexpr e)
          ^ string_of_sblock b
  | SWhile(e, b) ->
          "SWhile(" ^ (string_of_sexpr e) ^ ")" ^ string_of_sblock b
  | SCall(id,el) ->
          "SCall(" ^ id ^ ", params=["
          ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "])"
  | STCall(ptype,el) ->
          "STCall(" ^ (string_of_ptype ptype) ^ ", params=["
          ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "])"

and string_of_sexpr e =
    let (type_, sx) = e in
    "(# " ^ string_of_type type_ ^ " $ " ^ string_of_sx sx ^ " #)"

and string_of_svar v =
    let (hidden, id, type_, value) = v in
    "(" ^ (if hidden then "@" else "") ^ id ^ ":" ^ (string_of_type type_)
    ^ ", value=" ^
    (match value with Some e -> (string_of_sexpr e) | None -> "")
    ^  ")"

let string_of_sparam p =
    let (id, type_) = p in
    id ^ ":" ^ string_of_type type_

let string_of_spdecl = function
    SFunc(id, type_, params, body) ->
        "SFunc(" ^ id ^ ":" ^ (string_of_type type_)
        ^ ", params=[" ^ (String.concat ", " (List.map string_of_sparam params))
        ^ "])\n" ^ (string_of_sblock body)
  | STemplate(id, params, body) ->
        "STemplate(" ^ id
        ^ ", params=[" ^ (String.concat ", " (List.map string_of_sparam params))
        ^ "])\n" ^ (string_of_stblock body)
  | SGVar(v) ->
        "SGVar(" ^ (string_of_svar v) ^ ")"

let string_of_sprogram prog =
    String.concat "\n" (List.map string_of_spdecl prog)
    ^ "\n"

