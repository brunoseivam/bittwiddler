(* Abstract Syntax Tree *)

type id = Id of string

(* Types *)
type typename =
    TInt of (string * int * string)
  | TFloat of int
  | TString
  | TCustom of id
  | TNone

type op =
    Plus | Minus | Times | Div | Rem | LShift | RShift
  | BwOr | BwAnd | And | Or | Lt | LtEq | Eq | GtEq | Gt
type uop = BwNot | Not


type expr =
    LInt of int
  | LFloat of float
  | LString of string
  | Binop of expr * op * expr
  | Unop of op * expr
  | Match of expr * (expr * block) list
  | If of expr * expr list * expr list
  | For of expr * expr * expr list

type block_line =
    Var of expr * expr * expr
  | Expr of expr

type block = Block of expr list
type param = Param of id * typename
type parse = Parse of block

type pdecl =
    Template of id * param list * block
  | Func of id * typename * param list * block
  | Var

type program = Program of pdecl list * parse
