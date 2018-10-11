(* Abstract Syntax Tree *)

type op =
    Plus | Minus | Times | Div | Rem | LShift | RShift
  | BwOr | BwAnd | And | Or | Lt | LtEq | Eq | NEq | GtEq | Gt
  | Subscr | Access
type uop = BwNot | Not


type arm =
    Arm of expr * block

and block =
    Block of block_line list

and block_line =
    BDecl of pdecl
  | Expr of expr

and expr =
    LInt of int
  | LFloat of float
  | LString of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Match of expr * arm list
  | If of expr * block * block
  | For of expr * expr * block
  | Call of id * expr list
  | EId of id
  | EType of typename

and id =
  | Id of string

and typename =
  | TInt of (string * int * string)
  | TFloat of int
  | TString
  | TCustom of id
  | TNone

and pdecl =
    Template of id * param list * block
  | Func of id * typename * param list * block
  | Var of bool * id * expr option * expr option
  | TVar of expr * expr option * expr option

and param =
    Param of id * typename

type parse = Parse of block


type program = Program of pdecl list * parse
