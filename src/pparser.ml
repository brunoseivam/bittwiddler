open Ast

let string_of_id = function
    Id(id) -> id

let string_of_block = function
    Block(_) -> "\n{Block()}\n"

let string_of_typename = function
    TCustom(id) -> "Typename(" ^ string_of_id id ^ ")"
  | TInt((s,w,e)) -> "Typename(int, sign=" ^ s ^ ",width=" ^ string_of_int w ^
  ",endianess=" ^ e ^ ")"
  | TFloat(w) -> "Typename(float, width=" ^ string_of_int w ^ ")"
  | TNone -> "Typename(None)"

let string_of_param = function
    Param(id,typename) -> "Param(" ^ string_of_id id ^ "," ^ string_of_typename
typename ^ ")"

let string_of_params params =
    String.concat "," (List.map string_of_param params)

let string_of_pdecl = function
    Template(id, params, block) ->
        "Template(" ^ string_of_id id ^ ",params:" ^ string_of_params params
        ^ ")" ^ string_of_block block
  | Func(id, typename, params, block) ->
        "Function(" ^ string_of_id id
            ^ ",returns:" ^ string_of_typename typename
            ^ ",params:" ^ string_of_params params
        ^ ")" ^ string_of_block block

let string_of_pdecls pdecls =
    String.concat "\n" (List.map string_of_pdecl pdecls)

let string_of_parse = function
    Parse(block) -> "Parse(" ^ string_of_block block ^ ")"


let rec eval = function
    Program(pdecls, parse) ->
        (string_of_pdecls pdecls) ^ (string_of_parse parse)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
    let result = eval program in
    print_endline result
