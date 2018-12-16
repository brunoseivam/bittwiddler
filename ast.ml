(* Abstract Syntax Tree *)

type op =
    Plus | Minus | Times | Div | Rem | LShift | RShift
  | BwOr | BwAnd | And | Or | Lt | LtEq | Eq | NEq | GtEq | Gt
  | Subscr | Assign
type uop = BwNot | Not | Neg

type ptype =
  | TInt of (bool * int)
  | TFloat of int
  | TString
  | TBool
  | TAInt (* 'abstract' integer (no size info) *)
  | TAFloat (* 'abstract' float (no size info) *)
  | TNone

and var =
    Var of string * type_ option * expr option

and stmt =
    LVar of var (* local variable declaration *)
  | For of string * string * expr * stmt list
  | While of expr * stmt list
  | Expr of expr
  | Return of expr

and expr =
    LInt of int
  | LFloat of float
  | LString of string
  | LBool of bool
  | LArray of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Match of expr * (expr option * stmt list) list
  | Cond of (expr option * stmt list) list
  | Call of string * expr list

and type_ =
    ScalarType of ptype
  | ArrayType of ptype * expr option

and param =
    Param of string * type_

type program_decl =
    Func of string * type_ * param list * stmt list
  | GVar of var (* global variable *)

type program = Program of program_decl list * stmt list

(* Pretty-printing functions *)

let string_of_op = function
    Plus  -> "+"  | Minus  -> "-"   | Times  -> "*"  | Div    -> "/"
  | Rem   -> "%"  | LShift -> "<<"  | RShift -> ">>" | BwOr   -> "|"
  | BwAnd -> "&"  | And    -> "and" | Or     -> "or" | Lt     -> "<"
  | LtEq  -> "<=" | Eq     -> "=="  | NEq    -> "!=" | GtEq   -> ">="
  | Gt    -> ">"  | Subscr -> "[]"  | Assign -> "="

let string_of_uop = function
    BwNot -> "~" | Not -> "!" | Neg -> "-"

let rec string_of_ptype = function
    TInt(u, w) -> (if u then "uint" else "int") ^ string_of_int w
  | TFloat(w) -> "float" ^ string_of_int w
  | TString -> "string"
  | TBool -> "bool"
  | TAInt -> "int"
  | TAFloat -> "float"
  | TNone -> "None"

and string_of_expr = function
    LInt(i) -> string_of_int i
  | LFloat(f) -> string_of_float f
  | LString(s) -> "\"" ^ s ^ "\""
  | LBool(b) -> string_of_bool b
  | LArray(a) -> "[" ^ String.concat "," (List.map string_of_expr a) ^ "]"
  | Id(id) -> id
  | Binop(e1, Subscr, e2) ->
        string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
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
  | Call(id, exprs) ->
        id ^ "(" ^ String.concat "," (List.map string_of_expr exprs) ^ ")"

and string_of_block stmts =
    "{\n"
    ^ String.concat "\n" (List.map string_of_stmt stmts)
    ^ "\n}"

and string_of_stmt = function
    LVar v -> string_of_var v ^ ";"
  | While(e, b) -> "while " ^ string_of_expr e ^ string_of_block b
  | For(id1, id2, e, b) ->
        "for " ^ id1 ^ ", " ^ id2 ^ " in " ^ string_of_expr e ^ " "
        ^ string_of_block b
  | Expr e -> string_of_expr e ^ ";"
  | Return e -> "return " ^ string_of_expr e ^ ";"

and string_of_arm = function
        (Some e, b) -> string_of_expr e ^ " -> " ^ string_of_block b
      | (None, b)   -> "_ -> " ^ string_of_block b

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
    ScalarType ptype -> string_of_ptype ptype
  | ArrayType(ptype, count) ->
        string_of_ptype ptype
        ^ "["
        ^ (match count with Some(e) -> string_of_expr e | None -> "")
        ^ "]"

and string_of_param = function
    Param(id, type_) -> id ^ ":" ^ string_of_type type_

and string_of_params = function
    []     -> ""
  | params -> "(" ^ String.concat "," (List.map string_of_param params) ^ ") "

and string_of_var = function
    Var(id, type_, expr) ->
        "var "
        ^ id
        ^ (match type_ with Some(t) -> " : " ^ string_of_type t | None -> "")
        ^ (match expr with Some(e) -> " = " ^ string_of_expr e | None -> "")

let string_of_pdecl = function
    Func(id, type_, params, block) ->
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

