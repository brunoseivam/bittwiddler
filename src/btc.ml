open Ast

let _ =
    let lexbuf = Lexing.from_channel stdin in
    (* let _ = Parsing.set_trace true in *)
    let _ = Parser.program Scanner.token lexbuf in
    print_endline "Program accepted"


