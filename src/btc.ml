open Ast

let rec eval = function
    Program(pdecls, parse) ->
        (string_of_pdecls pdecls) ^ (string_of_parse parse)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
    let result = eval program in
    print_endline result
