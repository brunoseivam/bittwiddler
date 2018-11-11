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
  | TId of string

type arm =
    Arm of expr * block

and block =
    Block of block_line list

and block_line =
    BDecl of pdecl
  | Expr of expr
  | Return of expr

and if_ =
    If of expr option * block

and expr =
    LInt of int
  | LFloat of float
  | LString of string
  | LArray of expr list
  | EType of ptype
  | EId of id
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Match of expr * arm list
  | Cond of if_ list
  | For of id list * expr * block
  | While of expr * block
  | Call of id * expr list
  | TCall of ptype * expr list

and id =
    Id of string

and type_ =
    ScalarType of ptype * expr list option
  | ArrayType of ptype * expr list option * expr option

and pdecl =
    Template of string * param list * block
  | Func of id * type_ * param list * block
  | Var of bool * id * type_ option * expr option
  | TVar of expr * expr option * expr option

and param =
    Param of id * type_

type parse = Parse of block


type program = Program of pdecl list * parse

(* Pretty-printing functions *)

let string_of_op = function
    Plus  -> "+"  | Minus  -> "-"   | Times  -> "*"  | Div    -> "/"
  | Rem   -> "%"  | LShift -> "<<"  | RShift -> ">>" | BwOr   -> "|"
  | BwAnd -> "&"  | And    -> "and" | Or     -> "or" | Lt     -> "<"
  | LtEq  -> "<=" | Eq     -> "=="  | NEq    -> "!=" | GtEq   -> ">="
  | Gt    -> ">"  | Subscr -> "[]"  | Access -> "."  | Assign -> "="

let string_of_uop = function
    BwNot -> "~" | Not -> "!" | Neg -> "-"

let string_of_ptype = function
    TInt(u, w, e) -> u ^ "int" ^ string_of_int w ^ e
  | TFloat(w) -> "float" ^ string_of_int w
  | TId(id) -> id
  | TString -> "string"

let string_of_id = function
    Id(id) -> id

let rec string_of_expr = function
    LInt(i) -> string_of_int i
  | LFloat(f) -> string_of_float f
  | LString(s) -> "\"" ^ s ^ "\""
  | LArray(a) -> "[" ^ String.concat "," (List.map string_of_expr a) ^ "]"
  | EType(t) -> string_of_ptype t
  | EId(id) -> string_of_id id
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
        ^ String.concat "," (List.map string_of_id ids)
        ^ " in "
        ^ string_of_expr e ^ " "
        ^ string_of_block b
  | While(e, b) -> "while " ^ string_of_expr e ^ string_of_block b
  | Call(id, exprs) ->
        string_of_id id
        ^ "(" ^ String.concat "," (List.map string_of_expr exprs) ^ ")"
  | TCall(ptype,exprs) ->
        string_of_ptype ptype
        ^ "(" ^ String.concat "," (List.map string_of_expr exprs) ^ ")"

and string_of_block = function
        Block(lines) ->
            "{\n"
            ^ String.concat "\n" (List.map string_of_block_line lines)
            ^ "\n}"

and string_of_block_line = function
        BDecl(d) -> string_of_pdecl d ^ ";"
      | Expr(e) -> string_of_expr e ^ ";"
      | Return(e) -> "return " ^ string_of_expr e ^ ";"

and string_of_arm = function
        Arm(e, b) -> string_of_expr e ^ " -> " ^ string_of_block b

and string_of_arms (arms) =
        String.concat "\n" (List.map string_of_arm arms)

and string_of_cond = function
    If(Some e, b)::elses ->
        "if " ^ string_of_expr e ^ " " ^ string_of_block b
        ^ String.concat "" (List.map string_of_else elses)
  | _ -> ""

and string_of_else = function
    If(e, b) ->
        (match e with Some e -> "elif " ^ string_of_expr e ^ " "
                    | None -> "else ")
        ^ string_of_block b

and string_of_type = function
    ScalarType(ptype, args) ->
        string_of_ptype ptype
        ^ (match args with
            Some(a) -> "(" ^ String.concat "," (List.map string_of_expr a) ^ ")"
          | None -> "")
  | ArrayType(ptype, args, count) ->
        string_of_ptype ptype
        ^ (match args with
            Some(a) -> "(" ^ String.concat "," (List.map string_of_expr a) ^ ")"
          | None -> "")
        ^ "["
        ^ (match count with Some(e) -> string_of_expr e | None -> "")
        ^ "]"
and
    string_of_param = function
    Param(id, type_) -> string_of_id id ^ ":" ^ string_of_type type_
and
    string_of_params = function
    []     -> ""
  | params -> "(" ^ String.concat "," (List.map string_of_param params) ^ ") "
and
    string_of_pdecl = function
    Template(id, params, block) ->
        "template " ^ id ^ " "
        ^ string_of_params params
        ^ string_of_block block
  | Func(id, type_, params, block) ->
        "func "
        ^ string_of_id id ^ " "
        ^ string_of_params params
        ^ ":" ^ string_of_type type_ ^ " "
        ^ string_of_block block
  | Var(hidden, id, type_, expr) ->
        "var"
        ^ (match hidden with true -> "@ " | false -> " ")
        ^ string_of_id id
        ^ (match type_ with Some(t) -> " : " ^ string_of_type t | None -> "")
        ^ (match expr with Some(e) -> " = " ^ string_of_expr e | None -> "")
  | TVar(vid, vtype, vval) ->
        "var [ " ^ string_of_expr vid ^ " ]"
        ^ (match vtype with Some(t) -> " : [ " ^ string_of_expr t ^ " ]"
                         | None -> "")
        ^ (match vval with Some(v) -> " = " ^ string_of_expr v
                        | None -> "")

let string_of_parse = function
    Parse(block) -> "parse " ^ string_of_block block

let string_of_program = function
    Program(pdecls, parse) ->
        String.concat "\n" (List.map string_of_pdecl pdecls)
        ^ "\n"
        ^ string_of_parse parse

