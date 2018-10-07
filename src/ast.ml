(* Abstract Syntax Tree *)

type id = Id of string

(* Types *)
type typename =
    TCustom of id
  | TInt of (string * int * string)
  | TFloat of int
  | TNone

type block = Block of int list
type param = Param of id * typename
type parse = Parse of block

type pdecl =
    Template of id * param list * block
  | Func of id * typename * param list * block

type program = Program of pdecl list * parse
