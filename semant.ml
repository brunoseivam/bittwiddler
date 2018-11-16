(* Semantic checking for the BitTwiddler compiler *)

open Ast
open Sast

(* Look for duplicates in a list of names *)
let check_dup kind where names =
    let sorted = List.sort (compare) names in
    let rec dups = function
        [] -> ()
      | (a::b::_) when a = b ->
              raise (Failure ("duplicate " ^ kind ^ " in " ^ where ^ ": " ^ a))
      | _ :: t -> dups t
    in dups sorted

(* Semantic checking of the AST. Returns an SAST if successful,
 * throws an exception if something is wrong.
 *
 * Check each global declaration, then the declarations in parse.
 *)

let check prog =
    let pdecls, parse = match prog with
        Program(pdecls, parse) -> (pdecls, parse)
    in
    (* Check pdecls for duplicates *)
    let pdecl_name = function
        Template(id,_,_) -> id
      | Func(id,_,_,_) -> id
      | Var(_,id,_,_) -> id
      | TVar(_,_,_) -> raise (Failure ("can't declare TVar in pdecl"))
    in
    let pdecl_names = List.map (pdecl_name) pdecls in
    let _ = check_dup "pdecl" "globals" pdecl_names in

    (* For now, we expect a parse block with a lone emit call.
     * This is a "Hello, world!" compiler. *)
    match parse with
        Block([Expr(Call("emit", [LString(_)]))]) ->
            SProgram(pdecls, parse)
      | _ -> raise (Failure "expected parse { emit(\"a string\"); }")

