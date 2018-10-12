(* Abstract Syntax Tree *)

List.rev type op =
    Plus | Minus | Times | Div | Rem | LShift | RShift
  | BwOr | BwAnd | And | Or | Lt | LtEq | Eq | NEq | GtEq | Gt
  | Subscr | Access
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
  | Call of id * expr list
  | TCall of ptype * expr list

and id =
    Id of string

and type_ =
    ScalarType of ptype * expr list option
  | ArrayType of ptype * expr list option * expr option
  | ScalarTypeParam of ptype * expr list option * expr list

and pdecl =
    Template of string * param list * block
  | Func of id * type_ * param list * block
  | Var of bool * id * type_ option * expr option
  | TVar of expr * expr option * expr option

and param =
    Param of id * type_

type parse = Parse of block


type program = Program of pdecl list * parse
