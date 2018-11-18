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

(* Check bindings for duplicates and void types *)
let check_binds params where =
    let binds = List.map (function Param(id,type_) -> (id,type_)) params in
    let _ = check_dup "parameter" where (List.map fst binds) in
    List.iter (function
        (id,ScalarType(TNone)) ->
            raise (Failure ("illegal " ^ id ^ " : None in " ^ where))
      | (id,ArrayType(TNone,_)) ->
            raise (Failure ("illegal " ^ id ^ " : None[] in " ^ where))
      | _ -> ()
    ) binds

(* Semantic checking of the AST. Returns an SAST if successful,
 * throws an exception if something is wrong.
 *
 * Check each global declaration, then the declarations in main.
 *)

let check prog =
    let Program(pdecls, main) = prog in

    (* Add built-in functions and main to list of program declarations *)
    let built_in_funcs = [
        Func("emit", ScalarType(TNone), [], [])
    ] in
    let main_func = Func("main", ScalarType(TNone), [], main) in
    let pdecls = built_in_funcs @ pdecls @ [main_func] in

    (* Extract names from all declarations *)
    let pdecl_names = List.map (function
        Template(id,_,_) -> id
      | Func(id,_,_,_) -> id
      | GVar(Var(_,id,_,_)) -> id
    ) pdecls in

    (* Check for duplicates *)
    let _ = check_dup "declaration" "globals" pdecl_names in

    (* Check program declarations *)
    let _ = List.iter (function
        Func(id,_,params,_) ->
            check_binds params id
      | GVar(Var(true,id,_,_)) ->
            raise (Failure ("Global variables can't be hidden: " ^ id))
      | Template(id,params,_) ->
            check_binds params id
      | _ -> ()
    ) pdecls in

    (* For now, we expect a main block with a lone emit call.
     * This is a "Hello, world!" compiler. *)
    match main with
        [Expr(Call("emit", [LString(_)]))] ->
            SProgram(pdecls, main)
      | _ -> raise (Failure "expected main { emit(\"a string\"); }")

