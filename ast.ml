(* Abstract Syntax Tree *)

type op =
    Plus | Minus | Times | Div | Rem | LShift | RShift
  | BwOr | BwAnd | And | Or | Lt | LtEq | Eq | NEq | GtEq | Gt
  | Subscr | Access | Assign
type uop = BwNot | Not | Neg

type ptype =
  | TInt of (string * int * string)
  | TFloat of int
  | TString
  | TId of string * (expr list) option

and var =
    Var of bool * string * type_ option * expr option

and block_item =
    LVar of var (* local variable declaration *)
  | Expr of expr
  | Return of expr

and expr =
    LInt of int
  | LFloat of float
  | LString of string
  | LArray of expr list
  | Id of string
  | EType of ptype
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Match of expr * (expr * block_item list) list
  | Cond of (expr option * block_item list) list
  | For of string list * expr * block_item list
  | While of expr * block_item list
  | Call of string * expr list
  | TCall of ptype * expr list

and type_ =
    ScalarType of ptype
  | ArrayType of ptype * expr option

and param =
    Param of string * type_

type template_item =
    Field of var
  | TField of expr * expr option * expr option
  | TExpr of expr

type program_decl =
    Func of string * type_ * param list * block_item list
  | Template of string * param list * template_item list
  | GVar of var (* global variable *)

type program = Program of program_decl list * block_item list

(* Pretty-printing functions *)

let string_of_op = function
    Plus  -> "+"  | Minus  -> "-"   | Times  -> "*"  | Div    -> "/"
  | Rem   -> "%"  | LShift -> "<<"  | RShift -> ">>" | BwOr   -> "|"
  | BwAnd -> "&"  | And    -> "and" | Or     -> "or" | Lt     -> "<"
  | LtEq  -> "<=" | Eq     -> "=="  | NEq    -> "!=" | GtEq   -> ">="
  | Gt    -> ">"  | Subscr -> "[]"  | Access -> "."  | Assign -> "="

let string_of_uop = function
    BwNot -> "~" | Not -> "!" | Neg -> "-"

let rec string_of_targs = function
        Some(args) ->
            "(" ^ (String.concat "," (List.map string_of_expr args)) ^ ")"
      | None -> ""

and string_of_ptype = function
    TInt(u, w, e) -> u ^ "int" ^ string_of_int w ^ e
  | TFloat(w) -> "float" ^ string_of_int w
  | TId(id, args) -> id ^ string_of_targs args
  | TString -> "string"

and string_of_expr = function
    LInt(i) -> string_of_int i
  | LFloat(f) -> string_of_float f
  | LString(s) -> "\"" ^ s ^ "\""
  | LArray(a) -> "[" ^ String.concat "," (List.map string_of_expr a) ^ "]"
  | EType(t) -> string_of_ptype t
  | Id(id) -> id
  | Binop(e1, Subscr, e2) ->
        string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Binop(e1, Access, e2) ->
        string_of_expr e1 ^ "." ^ string_of_expr e2
  | Binop(e1, op, e2) ->
          string_of_expr e1
          ^ " " ^ string_of_op op
          ^ " " ^ string_of_expr e2
  | Unop(op, e) -> string_of_uop op ^ string_of_expr e
  | Match(e, arms) ->
          "match " ^ string_of_expr e ^ " {\n"
          ^ string_of_arms arms
          ^ "\n}"
  | Cond(conds) -> string_of_cond conds
  | For(ids, e, b) ->
        "for "
        ^ String.concat "," ids
        ^ " in "
        ^ string_of_expr e ^ " "
        ^ string_of_block b
  | While(e, b) -> "while " ^ string_of_expr e ^ string_of_block b
  | Call(id, exprs) ->
        id ^ "(" ^ String.concat "," (List.map string_of_expr exprs) ^ ")"
  | TCall(ptype,exprs) ->
        string_of_ptype ptype
        ^ "(" ^ String.concat "," (List.map string_of_expr exprs) ^ ")"

and string_of_block = function
        lines ->
            "{\n"
            ^ String.concat "\n" (List.map string_of_block_line lines)
            ^ "\n}"

and string_of_block_line = function
        LVar(v) -> string_of_var v ^ ";"
      | Expr(e) -> string_of_expr e ^ ";"
      | Return(e) -> "return " ^ string_of_expr e ^ ";"

and string_of_arm = function
        (e, b) -> string_of_expr e ^ " -> " ^ string_of_block b

and string_of_arms (arms) =
        String.concat "\n" (List.map string_of_arm arms)

and string_of_cond = function
    (Some e, b)::elses ->
        "if " ^ string_of_expr e ^ " " ^ string_of_block b
        ^ String.concat "" (List.map string_of_else elses)
  | _ -> ""

and string_of_else = function
    (e, b) ->
        (match e with Some e -> "elif " ^ string_of_expr e ^ " "
                    | None -> "else ")
        ^ string_of_block b

and string_of_type = function
    ScalarType(ptype) -> string_of_ptype ptype
  | ArrayType(ptype, count) ->
        string_of_ptype ptype
        ^ "["
        ^ (match count with Some(e) -> string_of_expr e | None -> "")
        ^ "]"
and
    string_of_param = function
    Param(id, type_) -> id ^ ":" ^ string_of_type type_
and
    string_of_params = function
    []     -> ""
  | params -> "(" ^ String.concat "," (List.map string_of_param params) ^ ") "
and
    string_of_var = function
    Var(hidden, id, type_, expr) ->
        "var"
        ^ (match hidden with true -> "@ " | false -> " ")
        ^ id
        ^ (match type_ with Some(t) -> " : " ^ string_of_type t | None -> "")
        ^ (match expr with Some(e) -> " = " ^ string_of_expr e | None -> "")


let string_of_tblock_line = function
    Field(v) -> string_of_var v ^ ";"
  | TExpr(e) -> string_of_expr e ^ ";"
  | TField(vid, vtype, vval) ->
        "var [ " ^ string_of_expr vid ^ " ]"
        ^ (match vtype with Some(t) -> " : [ " ^ string_of_expr t ^ " ]"
                         | None -> "")
        ^ (match vval with Some(v) -> " = " ^ string_of_expr v
                        | None -> "")

let string_of_tblock = function
        lines ->
            "{\n"
            ^ String.concat "\n" (List.map string_of_tblock_line lines)
            ^ "\n}"

let string_of_pdecl = function
    Template(id, params, block) ->
        "template " ^ id ^ " "
        ^ string_of_params params
        ^ string_of_tblock block
  | Func(id, type_, params, block) ->
        "func " ^ id ^ " "
        ^ string_of_params params
        ^ ":" ^ string_of_type type_ ^ " "
        ^ string_of_block block
  | GVar(v) ->
        string_of_var v

let string_of_program = function
    Program(pdecls, block) ->
        String.concat "\n" (List.map string_of_pdecl pdecls)
        ^ "\n"
        ^ "main" ^ string_of_block block

