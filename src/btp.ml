(* btp: BitTwiddler Parser *)
open Ast

let _ =
    let trace = Array.length Sys.argv > 1 && Sys.argv.(1) = "-v" in
    let _ = Parsing.set_trace trace in
    let lexbuf = Lexing.from_channel stdin in
    let _ = Parser.program Scanner.token lexbuf in
    print_endline "Program accepted"


