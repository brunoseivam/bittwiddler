(* Semantically-checked Abstract Syntax Tree *)

open Ast

type stype =
    SScalar of ptype
  | SArray of ptype * sexpr option

and sexpr = stype * sx

and sx =
    SLInt of int
  | SLFloat of float
  | SLString of string
  | SLBool of bool
  | SLArray of sexpr list
  | SId of string
  | SEType of ptype
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SCall of string * sexpr list
  | STCall of ptype * sexpr list

and sstmt =
    SLVar of svar (* local variable declaration *)
  | SExpr of sexpr
  | SReturn of sexpr
  | SFor of svar * svar * sexpr * sstmt list
  | SWhile of sexpr * sstmt list

and svar = string * stype * sexpr option

type sparam = string * stype

type stemplate_item =
    SField of svar
  | STField of sexpr * sexpr option * sexpr option
  | STExpr of sexpr

type sfunc = string * stype * sparam list * sstmt list
type stempl = string * sparam list * stemplate_item list

type sprogram_decl =
    SFunc of sfunc
  | STemplate of stempl
  | SGVar of svar (* global variable *)

type sprogram = sprogram_decl list

let size_t = SScalar (TInt(true,64))
let char_t = SScalar (TInt(false,8))

let is_integer = function
    SScalar(TInt(_,_)) | SScalar TAInt -> true
  | _ -> false

let is_float = function
    SScalar (TFloat _) | SScalar TAFloat -> true
  | _ -> false

let is_number x = (is_integer x) || (is_float x)

let is_bool = function SScalar TBool -> true | _ -> false

(* Pretty-printing *)

let rec string_of_stype = function
    SScalar ptype -> string_of_ptype ptype
  | SArray (ptype, count) ->
        string_of_ptype ptype
        ^ "["
        ^ (match count with Some(e) -> string_of_sexpr e | None -> "")
        ^ "]"
and string_of_ststmt = function
    SField(v) -> (string_of_svar v) ^ ";"
  | STField(e1, e2, e3) ->
        let e1 = (string_of_sexpr e1) in
        let e2 = (match e2 with Some e -> (string_of_sexpr e) | None -> "") in
        let e3 = (match e3 with Some e -> (string_of_sexpr e) | None -> "") in
        "STField(id=" ^ e1 ^ ", type=" ^ e2 ^ ", value=" ^ e3 ^ ");"
  | STExpr(e) -> (string_of_sexpr e) ^ ";"

and string_of_stblock b =
    "{\n" ^ (String.concat "\n" (List.map string_of_ststmt b))

and string_of_sstmt = function
    SLVar(v) -> string_of_svar v ^ ";"
  | SExpr(e) -> string_of_sexpr e ^ ";"
  | SReturn(e) -> "return " ^ string_of_sexpr e ^ ";"
  | SFor(idx_sv, item_sv, e, b) ->
          "for "
          ^ string_of_svar idx_sv ^ ","
          ^ string_of_svar item_sv ^ " in "
          ^ string_of_sexpr e
          ^ string_of_sblock b
  | SWhile(e, b) ->
          "while " ^ string_of_sexpr e ^ ")" ^ string_of_sblock b

and string_of_sblock b =
    "{\n" ^ (String.concat "\n" (List.map string_of_sstmt b)) ^ "\n}\n"

and string_of_sarm arm =
    let (e, block) = arm in
    (string_of_sexpr e) ^ " -> " ^ (string_of_sblock block)

and string_of_sx = function
    SLInt i -> string_of_int i
  | SLFloat f -> string_of_float f
  | SLString s -> "\"" ^ s ^ "\""
  | SLBool b -> string_of_bool b
  | SLArray lx -> "[" ^ (String.concat "," (List.map string_of_sexpr lx)) ^ "]"
  | SId id -> id
  | SEType t -> "SEType(" ^ (string_of_ptype t) ^ ")"
  | SBinop(e1,Subscr,e2) ->
          string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SBinop(e1,op,e2) ->
          string_of_sexpr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_sexpr e2
  | SUnop(uop,e) ->
          string_of_uop uop ^ " " ^ string_of_sexpr e
  | SIf(pred,then_,else_) ->
        "if " ^ string_of_sexpr pred
        ^ string_of_sblock then_
        ^ "else " ^ string_of_sblock else_
  | SCall(id,el) ->
          id ^ " ("
          ^ (String.concat ", " (List.map string_of_sexpr el)) ^ ")"
  | STCall(ptype,el) ->
          "STCall(" ^ (string_of_ptype ptype) ^ ", params=["
          ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "])"

and string_of_sexpr (stype,sx) =
    "(" ^ string_of_sx sx ^ "):" ^ string_of_stype stype

and string_of_svar (id, stype, value) =
    "var " ^ id ^ ":" ^ string_of_stype stype ^ " = "
    ^ (match value with Some e -> string_of_sexpr e | None -> "")

let string_of_sparam (id, stype) =
    id ^ ":" ^ string_of_stype stype

let string_of_spdecl = function
    SFunc(id, stype, params, body) ->
        "func " ^ id ^ ":" ^ string_of_stype stype
        ^ " (" ^ (String.concat ", " (List.map string_of_sparam params))
        ^ ")\n" ^ string_of_sblock body
  | STemplate(id, params, body) ->
        "STemplate(" ^ id
        ^ ", params=[" ^ (String.concat ", " (List.map string_of_sparam params))
        ^ "])\n" ^ (string_of_stblock body)
  | SGVar(v) ->
        "SGVar(" ^ (string_of_svar v) ^ ")"

let string_of_sprogram prog =
    String.concat "\n" (List.map string_of_spdecl prog)
    ^ "\n"

