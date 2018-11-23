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

    (* Array literal: check that all expressions in the list are of the same
     * type; return that type *)
    let rec type_of_arr_lit = function
        [] -> raise (Failure ("Can't determine type of empty literal array"))
      | [(ScalarType(t),_)] -> t
      | (ScalarType(t),_)::tl when t = type_of_arr_lit tl -> t
      | _ -> raise (Failure ("Array literal has mixed or invalid types"))
    in

    (* Type compatibility: 'abstract' types are promoted to concrete types *)
    (* TODO: upcast for different integer/float sizes *)
    let check_type_compat t1 t2 =
        let failure = Failure (
           "Incompatible types " ^ string_of_type t1
           ^ " and " ^ string_of_type t2
        ) in

        let upcast t1 t2 = match (t1,t2) with
            (TInt(_,_,_), TAInt) -> (t1, t1)
          | (TAInt, TInt(_,_,_)) -> (t2, t2)
          | (TFloat(_), TAFloat) -> (t1, t1)
          | (TAFloat, TFloat(_)) -> (t2, t2)
          | (_, _) when t1 = t2 -> (t1, t2)
          | _ -> raise failure
        in

        match (t1, t2) with
            (ScalarType(st1), ScalarType(st2)) ->
                let (st1', st2') = upcast st1 st2 in
                    (ScalarType(st1'), ScalarType(st2'))
          | (ArrayType(st1,l1), ArrayType(st2,l2)) when l1=l2 ->
                let (st1', st2') = upcast st1 st2 in
                    (ArrayType(st1',l1), ArrayType(st2',l2))
          | _ -> raise failure
    in

    let is_array = function
        ArrayType(_,_) -> true
      | _ -> false
    in

    let is_integer = function
        ScalarType(TInt(_,_,_)) | ScalarType(TAInt) -> true
      | _ -> false
    in

    let is_float = function
        ScalarType(TFloat(_)) | ScalarType(TAFloat) -> true
      | _ -> false
    in

    let is_number x = (is_integer x) || (is_float x) in

    (* Return a semantically-checked expression *)
    let rec expr e = match e with
        LInt l -> (ScalarType(TAInt), SLInt l)
      | LFloat l -> (ScalarType(TAFloat), SLFloat l)
      | LString l -> (ScalarType(TString), SLString l)
      | LArray el ->
            (* sel = semantically-checked expression list *)
            let sel = List.map expr el in
            (ArrayType(type_of_arr_lit sel, Some(LInt(List.length sel))),
             SLArray sel)

(*      | Id s -> (type_of_id s, SId s)*)
      | EType t -> (ScalarType TType , SEType t)
      | Binop(e1,op,e2) ->
            let failure t1 t2 = Failure (
                "Operator " ^ string_of_op op ^ " not defined for types "
                ^ string_of_type t1 ^ " and " ^ string_of_type t2
            ) in

            let (t1, e1') = expr e1
            and (t2, e2') = expr e2 in

            (* Promote types and check that they are compatible *)
            let (t1', t2') = match op with
                Subscr when is_array t1 && is_integer t2 -> (t1, t2)
              | _ -> check_type_compat t1 t2
            in

            let ty = match op with
                (* Overloaded Plus *)
                Plus -> (match (t1', t2') with
                    (* String concatenation *)
                    (ScalarType(TString), _) -> t1'
                    (* Array concatenation *)
                  | (ArrayType(st1',Some(LInt(l1))),
                     ArrayType(_,Some(LInt(l2)))) ->
                        ArrayType(st1',Some(LInt(l1+l2)))
                    (* Number addition *)
                  | (t1',_) when is_number t1' -> t1'
                  | _ -> raise (failure t1' t2'))

                (* Arithmetic: defined for numbers *)
              | Minus | Times | Div when is_number t1' -> t1'
                (* Defined for Integers only *)
              | Rem | LShift | RShift | BwOr | BwAnd when is_integer t1' -> t1'
                (* Boolean operations return smallest unsigned integer *)
              | And | Or | Lt | LtEq | Eq | NEq | GtEq | Gt -> boolean
                (* Array subscript: returned type is the type of a single
                 * element *)
              | Subscr -> let ArrayType(at,_) = t1' in ScalarType(at)
              | Assign -> t2'
                (* TODO: Access operator *)
              | _ -> raise (failure t1' t2')
            in
            (ty, SBinop((t1',e1'), op, (t2',e2')))
        | Call(id,el) -> (ScalarType TNone, SCall(id, List.map expr el))
        | _ -> raise (Failure ("Unimplemented: " ^ string_of_expr e))
    in

    let sblock_item = function
        Expr(ex) -> let (t, e) = expr ex in SExpr(t, e)
      | _ -> raise (Failure ("Unimplemented"))
    in

    let smain = List.map sblock_item main in

    (* For now, we expect a main block with expressions *)
    SProgram([], smain)

