(* Semantic checking for the BitTwiddler compiler *)

open Ast
open Sast
open Emit

module StringMap = Map.Make(String)

type context = {
    variables : svar StringMap.t;
    functions : sprogram_decl StringMap.t;
    templates : sprogram_decl StringMap.t;
}

(* Helper functions to add elements to maps *)
let add_to_map map id elem kind =
    match id with
        _ when StringMap.mem id map ->
            raise (Failure ("duplicate " ^ kind ^ " declaration: " ^ id))
      | _ -> StringMap.add id elem map

let add_var map v = match v with
    SVar(_,id,_,_) -> add_to_map map id v "variable"

let add_func map f = match f with
    SFunc(id,_,_,_) -> add_to_map map id f "function"
  | _ -> map

let add_templ map t = match t with
    STemplate(id,_,_) -> add_to_map map id t "template"
  | _ -> map

let find_elem map id =
    try StringMap.find id map
    with Not_found -> raise (Failure ("undeclared identifier " ^ id))

(* Built-in functions transformations *)
let check_emit_fmt ctx emit_fmt =
    let emit_err id t =
        raise (Failure ("don't know how to emit " ^ id ^ ":"
                        ^ string_of_type t))
    in

    (* Builds the printf-ready format string and list of variable ids *)
    let rec build_args fmt args exprs = match exprs with
        [] -> (fmt, args)
      | (STR s)::tl -> build_args (fmt ^ s) args tl
      | (VAR id)::tl -> (match find_elem ctx.variables id with
            SVar(_,id,ScalarType pt,_) ->
                let tfmt = (match pt with
                    TInt("u",8,_) | TInt("u",16,_) | TInt("u",32,_) -> "%u"
                  | TAInt | TInt("",8,_) | TInt("",16,_) | TInt("",32,_) -> "%d"
                  | TInt("u",64,_) -> "%lu"
                  | TInt("",64,_) -> "%ld"
                  | TAFloat | TFloat(32) | TFloat(64) -> "%f"
                  | TString -> "%s"
                  | _ -> emit_err id (ScalarType pt)
                ) in
                build_args (fmt ^ tfmt) ((ScalarType pt, SId id)::args) tl
          | SVar(_,id,t,_) -> emit_err id t
        )
    in

    (* Note: args is reversed here *)
    let (fmt, args) = build_args "" [] (parse_emit_fmt emit_fmt) in
    ([(ScalarType TString, SLString fmt)] @ (List.rev args))


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

let is_array = function
    ArrayType(_,_) -> true
  | _ -> false

let is_integer = function
    ScalarType(TInt(_,_,_)) | ScalarType(TAInt) -> true
  | _ -> false

let is_float = function
    ScalarType(TFloat(_)) | ScalarType(TAFloat) -> true
  | _ -> false

let is_number x = (is_integer x) || (is_float x)

let rec type_of_arr_lit = function
    [] -> raise (Failure ("Can't determine type of empty literal array"))
  | [(ScalarType(t),_)] -> t
  | (ScalarType(t),_)::tl when t = type_of_arr_lit tl -> t
  | _ -> raise (Failure ("Array literal has mixed or invalid types"))

(*
 * Checkers that return semantically checked elements
 *)

(* Check expression. Return sexpr. *)
let rec check_expr ctx e = match e with
    LInt l -> (ScalarType(TAInt), SLInt l)
  | LFloat l -> (ScalarType(TAFloat), SLFloat l)
  | LString l -> (ScalarType(TString), SLString l)
  | LArray el ->
        (* sel = semantically-checked expression list *)
        let sel = List.map (check_expr ctx) el in
            (ArrayType(type_of_arr_lit sel, Some(LInt(List.length sel))),
            SLArray sel)
  | Id s ->
        let SVar(_,_,type_,_) = find_elem ctx.variables s in (type_, SId s)
  | EType t -> (ScalarType TType , SEType t)
  | Binop(e1,op,e2) ->
        let failure t1 t2 = Failure (
            "Operator " ^ string_of_op op ^ " not defined for types "
            ^ string_of_type t1 ^ " and " ^ string_of_type t2
        ) in

        let (t1, e1') = check_expr ctx e1
        and (t2, e2') = check_expr ctx e2 in

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
          | Subscr -> (match t1' with
                ArrayType(at,_) -> ScalarType(at)
              | _ -> raise (Failure (
                  "Operator " ^ string_of_op op ^ " used on non-array")))
              | Assign -> t2'
                (* TODO: Access operator *)
              | _ -> raise (failure t1' t2')
            in
            (ty, SBinop((t1',e1'), op, (t2',e2')))

        (* When 'emit' is called, we have to build its arguments from the
         * format string *)
        | Call("emit", el) -> (match el with
            [(LString s)] ->
                (ScalarType TNone, SCall("emit", check_emit_fmt ctx s))
          | _ -> raise (Failure ("emit requires a single literal "
                                 ^ "string argument")))
        | Call(id, el) -> (ScalarType TNone, SCall(id, List.map (check_expr ctx) el))
        | _ -> raise (Failure ("Not implemented: " ^ string_of_expr e))

let check_var ctx v =
    let Var(hidden, id, t, e) = v in
    let (t, e) = match (t, e) with
        (* Both type and initial value, check that types are compatible *)
        (Some t, Some e) ->
            let (st, se) = check_expr ctx e in
            let (t', st') = check_type_compat t st in
            (t', (st', se))
        (* Only type was declared, add automatic input reading (TODO)*)
      | (Some _, None) ->
            raise (Failure ("Not implemented"))
        (* Only value was declared, coerce type *)
      | (None, Some e) ->
            let (st, se) = check_expr ctx e in
            (st, (st, se))
      | (None, None) ->
            raise (Failure ("")) (*TODO: meaningful error *)
    in
    let sv = SVar(hidden, id, t, Some e) in
    ({ ctx with variables = add_var ctx.variables sv }, sv)

(* Returns a new context and a semantically checked block item *)
let check_block_item ctx bitem = match bitem with
    LVar(v) -> let (ctx, sv) = check_var ctx v in (ctx, SLVar(sv))
  | Expr(e) -> (ctx, SExpr(check_expr ctx e))
  | Return(e) -> (ctx, SReturn(check_expr ctx e))

let rec check_block ctx block = match block with
    [] -> []
  | hd::tl ->
        let (ctx, item) = check_block_item ctx hd in
        item::(check_block ctx tl)

(* Look for duplicates in a list of names *)
let check_dup kind where names =
    let sorted = List.sort (compare) names in
    let rec dups = function
        [] -> ()
      | (a::b::_) when a = b ->
              raise (Failure ("duplicate " ^ kind ^ " in " ^ where ^ ": " ^ a))
      | _ :: t -> dups t
    in dups sorted

(* Check bindings for duplicates and None types.
 * Returns a new context with the parameters as local variables and a list of
 * semantically checked parameters *)
let rec add_params ctx params = match params with
    [] -> ctx
  | Param(id, type_)::tl ->
        let sv = SVar(false, id, type_, None) in
        let ctx = { ctx with variables = add_var ctx.variables sv } in
        add_params ctx tl

let check_params ctx params where =
    let _ = check_dup "parameter" where
        (List.map (function Param(id,_) -> id) params)
    in
    let _ = List.iter (function
        Param(id,ScalarType(TNone)) ->
            raise (Failure ("illegal " ^ id ^ " : None in " ^ where))
      | Param(id,ArrayType(TNone,_)) ->
            raise (Failure ("illegal " ^ id ^ " : None[] in " ^ where))
      | _ -> ()
    ) params in

    (add_params ctx params,
    List.map (function Param(id, type_) -> SParam(id, type_)) params)

(* Check global program declarations: Global variable, function and template.
 * Returns a modified context and a semantically checked program declaration.
 *)
let check_pdecl ctx decl = match decl with
    Func(id, type_, params, body) ->
        let (lctx, sp) = check_params ctx params id in
        let sf = SFunc(id, type_, sp, List.rev (check_block lctx body)) in
        ({ ctx with functions = add_func ctx.functions sf }, sf)
  | Template(_, _, _) ->
        raise (Failure ("Not implemented")) (* TODO
        let st = STemplate(id, check_params params, check_tblock ctx body) in
        ({ ctx with templates = add_templ ctx.templates st}, st)*)
  | GVar(Var(true,id,_,_)) ->
        raise (Failure ("Global variables can't be hidden: " ^ id))
  | GVar(v) ->
        let (ctx, sv) = check_var ctx v in
        (ctx, SGVar(sv))

let rec check_pdecls ctx pdecls = match pdecls with
    [] -> []
  | hd::tl -> let (ctx, d) = check_pdecl ctx hd in d::(check_pdecls ctx tl)

(* Semantic checking of the AST. Returns an SAST if successful,
 * throws an exception if something is wrong.
 *
 * Check each global declaration, then the declarations in main.
 *)

let check prog =
    let Program(pdecls, main) = prog in

    (* Add built-in functions and main to list of program declarations *)
    let built_in_funcs = [
        Func("emit", ScalarType(TNone), [], []);
        Func("main", ScalarType(TInt("",32,"")), [], main @ [Return(LInt(0))])
    ] in
    let pdecls = built_in_funcs @ pdecls in

    (* Build global maps *)
    let ctx = {
        variables = StringMap.empty;
        functions = StringMap.empty;
        templates = StringMap.empty;
    } in

    SProgram(List.rev (check_pdecls ctx pdecls))
