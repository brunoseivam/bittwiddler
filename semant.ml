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
 * Check each global declaration, then the declarations in main.
 *)

let check prog =
    let Program(pdecls, main) = prog in

    (* Check program_decls for duplicates *)
    let pdecl_name = function
        Template(id,_,_) -> id
      | Func(id,_,_,_) -> id
      | GVar(Var(_,id,_,_)) -> id
    in
    let pdecl_names = List.map (pdecl_name) pdecls in
    let _ = check_dup "Global Declaration" "globals" pdecl_names in

    (* Check for invalid global variables *)
    let check_gvar = function
        GVar(Var(true,id,_,_)) ->
            raise (Failure ("Global variables can't be hidden: " ^ id))
      | _ -> ()
    in
    let _ = List.map (check_gvar) pdecls in

    (* For now, we expect a main block with a lone emit call.
     * This is a "Hello, world!" compiler. *)
    match main with
        [Expr(Call("emit", [LString(_)]))] ->
            SProgram(pdecls, main)
      | _ -> raise (Failure "expected main { emit(\"a string\"); }")

