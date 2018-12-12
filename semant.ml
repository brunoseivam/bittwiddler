(* Semantic checking for the BitTwiddler compiler *)

open Ast
open Sast
open Emit

module StringMap = Map.Make(String)

type context = {
    variables : svar StringMap.t;
    functions : sfunc StringMap.t;
    templates : stempl StringMap.t;
}

(* Shorthand *)
let fail s = raise (Failure s)

let string_of_ctx ctx =
    let string_of_sfunc (f:sfunc) = string_of_spdecl (SFunc f) in
    let string_of_stempl (t:stempl) = string_of_spdecl (STemplate t) in

    let fold f m = StringMap.fold (fun k v r -> k^":"^(f  v)^"\n"^r) m "" in

    "context\n\n"
    ^ "variables\n---------\n\n" ^ (fold (string_of_svar) ctx.variables)
    ^ "functions\n---------\n\n" ^ (fold (string_of_sfunc) ctx.functions)
    ^ "templates\n---------\n\n" ^ (fold (string_of_stempl) ctx.templates)

(* Helper functions to add elements to maps *)
let add_to_map map id elem kind =
    match id with
        _ when StringMap.mem id map ->
            fail ("duplicate " ^ kind ^ " declaration: " ^ id)
      | _ -> StringMap.add id elem map

let add_var map = function
    (id,_,_) as v -> add_to_map map id v "variable"

let add_func map = function
    (id,_,_,_) as f -> add_to_map map id f "function"

let add_templ map = function
    (id,_,_) as t -> add_to_map map id t "template"

let find_elem map id =
    try StringMap.find id map
    with Not_found -> fail ("undeclared identifier " ^ id)

(* Built-in functions transformations *)
let check_emit_fmt fname ctx emit_fmt =
    let emit_err id t =
        fail ("don't know how to emit " ^ id ^ ":" ^ string_of_type t)
    in

    let fkind = (ScalarType (TInt(false, 32)), SLInt(match fname with
        "emit" -> 0 | "print" -> 1 | "fatal" -> 2
      | _ -> fail ("invalid 'emit' kind: '" ^ fname ^ "'")))
    in

    (* Builds the printf-ready format string and list of variable ids *)
    let rec build_args fmt args exprs = match exprs with
        [] -> (fmt, args)
      | (STR s)::tl -> build_args (fmt ^ s) args tl
      | (VAR id)::tl -> (match find_elem ctx.variables id with
            (id,ScalarType pt,_) ->
                let tfmt = (match pt with
                    TInt(true,64) -> "%lu"
                  | TInt(false,64) -> "%ld"
                  | TInt(true,_) -> "%u"
                  | TAInt | TInt(false,_) -> "%d"
                  | TAFloat | TFloat(_) -> "%f"
                  | TString -> "%s"
                  | TBool -> "%d"
                  | _ -> emit_err id (ScalarType pt)
                ) in
                build_args (fmt ^ tfmt) ((ScalarType pt, SId id)::args) tl
          | (id,t,_) -> emit_err id t
        )
    in

    (* Note: args is reversed here *)
    let (fmt, args) = build_args "" [] (parse_emit_fmt emit_fmt) in
    ([fkind; (ScalarType TString, SLString fmt)] @ (List.rev args))


(* Type compatibility: 'abstract' types are promoted to concrete types *)
(* TODO: upcast for different integer/float sizes *)
let check_type_compat t1 t2 =
    let failure = "Incompatible types " ^ string_of_type t1
        ^ " and " ^ string_of_type t2
    in

    let upcast t1 t2 = match (t1,t2) with
        (TInt(_,_), TAInt) -> (t1, t1)
      | (TAInt, TInt(_,_)) -> (t2, t2)
      | (TFloat(_), TAFloat) -> (t1, t1)
      | (TAFloat, TFloat(_)) -> (t2, t2)
      | (_, _) when t1 = t2 -> (t1, t2)
      | _ -> fail failure
    in

    match (t1, t2) with
        (ScalarType(st1), ScalarType(st2)) ->
            let (st1', st2') = upcast st1 st2 in
                (ScalarType(st1'), ScalarType(st2'))
      | (ArrayType(st1,l1), ArrayType(st2,l2)) when l1=l2 ->
            let (st1', st2') = upcast st1 st2 in
                (ArrayType(st1',l1), ArrayType(st2',l2))
      | _ -> fail failure

let is_array = function
    ArrayType(_,_) -> true
  | _ -> false

let is_integer = function
    ScalarType(TInt(_,_)) | ScalarType(TAInt) -> true
  | _ -> false

let is_float = function
    ScalarType(TFloat(_)) | ScalarType(TAFloat) -> true
  | _ -> false

let is_number x = (is_integer x) || (is_float x)

let is_bool = function ScalarType TBool -> true | _ -> false

let rec type_of_arr_lit = function
    [] -> fail "Can't determine type of empty literal array"
  | [(ScalarType(t),_)] -> t
  | (ScalarType(t),_)::tl when t = type_of_arr_lit tl -> t
  | _ -> fail "Array literal has mixed or invalid types"

(*
 * Checkers that return semantically checked elements
 *)

(* Check expression. Return sexpr. *)
let rec check_expr ctx = function
    LInt l -> (ScalarType(TAInt), SLInt l)
  | LFloat l -> (ScalarType(TAFloat), SLFloat l)
  | LString l -> (ScalarType(TString), SLString l)
  | LBool l -> (ScalarType TBool, SLBool l)
  | LArray el ->
        (* sel = semantically-checked expression list *)
        let sel = List.map (check_expr ctx) el in
            (ArrayType(type_of_arr_lit sel, Some(LInt(List.length sel))),
            SLArray sel)
  | Id s ->
        let (_,type_,_) = find_elem ctx.variables s in (type_, SId s)
  | EType t -> (ScalarType TType , SEType t)
  | Binop(e1,op,e2) ->
        let failure t1 t2 =
            "Operator " ^ string_of_op op ^ " not defined for types "
            ^ string_of_type t1 ^ " and " ^ string_of_type t2
        in

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
              | _ -> fail (failure t1' t2'))

            (* Arithmetic: defined for numbers *)
          | Minus | Times | Div when is_number t1' -> t1'
            (* Defined for Integers only *)
          | Rem | LShift | RShift | BwOr | BwAnd when is_integer t1' -> t1'
            (* Boolean operations *)
          | And | Or when is_bool t1' -> ScalarType TBool
          | Lt | LtEq | Eq | NEq | GtEq | Gt -> ScalarType TBool

            (* Array subscript: returned type is the type of a single
             * element *)
          | Subscr -> (match t1' with
                ArrayType(at,_) -> ScalarType(at)
              | _ -> fail ("Operator " ^ string_of_op op
                           ^ " used on non-array"))
          | Assign -> t2'
            (* TODO: Access operator *)
          | _ -> fail (failure t1' t2')
        in
        (ty, SBinop((t1',e1'), op, (t2',e2')))

  | Unop(uop, e) ->
        let (t, e') = check_expr ctx e in
        let ty = match (t, uop) with
            (ScalarType TBool, Not) -> t
          | (ScalarType (TInt _), BwNot)
          | (ScalarType (TInt _), Neg)
          | (ScalarType (TFloat _), Neg) -> t
          | _ -> fail ("Operator " ^ (string_of_uop uop)
                                 ^ " not defined for type "
                                 ^ (string_of_type t))
        in
        (ty, SUnop(uop, (t, e')))

    (* Match will be transformed into Cond *)
  | Match(matching, arms) ->
        let to_cond = function
            (Some m, block) -> (Some(Binop(matching, Eq, m)), block)
          | (None, block) -> (None, block)
        in
        check_expr ctx (Cond(List.map to_cond arms))

  | Cond(conds) ->
        (* conds is (expr option * block_item list) list
         *           vvvvvvvvvvv   vvvvvvvvvvvvvvv
         *            condition         block
         *
         * So we iterate over conds and enforce the following constraints:
         *   - All conditions must have boolean type
         *   - All blocks must have the same type
         *
         * Then we do a transformation to make it easy for codegen:
         *
         *   if (pred1) {B1} else if(pred2) {B2} else {B3}
         *
         * Becomes:
         *
         *   if (pred1) {B1} else { if(pred2) {B2} else {B3} }
         *)

        (* An expression fed to an if or else if clause must be boolean *)
        let check_cond_expr = function
            None -> None
          | Some e ->
                (match (check_expr ctx e) with
                    (ScalarType TBool,_) as e' -> Some e'
                  | (_,sx) -> fail ("Non-boolean expression in conditional: "
                                    ^ string_of_sx sx)
                )
        in

        let rec check_conds = function
            [] -> fail "internal error: empty conditional?"
          | [(e, b)] ->
                let (se, (t, sb)) = (check_cond_expr e, check_block ctx b) in
                (t, [(se, sb)])
          | (e, b)::tl ->
                let (se, (t1, sb)) = (check_cond_expr e, check_block ctx b) in
                let (t2, r) = check_conds tl in
                if t1=t2 then
                    (t1, (se, sb)::r)
                else
                    fail "conditional blocks have different types"
        in

        (* Transform into simpler nested if/else blocks *)
        let rec simplify_conds t = function
            [(Some se,sb)] -> SIf(se, sb, [])
          | [(Some se1,sb1);(None,sb2)] -> SIf(se1, sb1, sb2)
          | (Some se, sb)::tl -> SIf(se, sb, [SExpr(t, simplify_conds t tl)])
          | _ -> fail "internal error: malformed conditional"
        in

        let (t, sconds) = check_conds conds in
        (t, simplify_conds t sconds)

  | While(pred, block) ->
        let pred' = match (check_expr ctx pred) with
            (ScalarType TBool, _) as pred' -> pred'
          | (_,sx) -> fail ("Non-boolean expression in while predicate: "
                            ^ (string_of_sx sx))
        in

        let (t, block') = check_block ctx block in
        (t, SWhile(pred', block'))

  (* When 'emit' is called, we have to build its arguments from the format
   * string *)
  | Call(id, el) -> (match id with
        "emit" | "print" | "fatal" ->
            (match el with
                [(LString s)] ->
                    (ScalarType TNone, SCall("__bt_emit",
                                             check_emit_fmt id ctx s))
              | _ -> fail ("'" ^ id ^ "' requires a single literal "
                                     ^ "string argument"))
      | _ ->
            let (_,type_,_,_) = find_elem ctx.functions id in
            (type_, SCall(id, List.map (check_expr ctx) el)))

  | _  as e -> fail ("Not implemented: " ^ string_of_expr e)

and check_var ctx v =
    let Var(id, t, e) = v in
    let (t, e) = match (t, e) with
        (* Both type and initial value, check that types are compatible *)
        (Some t, Some e) ->
            let (st, se) = check_expr ctx e in
            let _ = check_type_compat t st in
            (t, (st, se))

        (* Only type was declared, add automatic input reading *)
      | (Some t, None) -> (t, (t, SCall("__bt_read", [])))

        (* Only value was declared, coerce type *)
      | (None, Some e) ->
            let (st, se) = check_expr ctx e in
            (match st with
                ScalarType TAInt | ScalarType TAFloat ->
                    fail ("can't determine type of variable " ^ id)
              | _ ->
                    (st, (st, se))
            )
      | (None, None) ->
            fail ("internal error: variable " ^ id
                  ^ " has no type and no value")
    in
    let sv = (id, t, Some e) in
    ({ ctx with variables = add_var ctx.variables sv }, sv)

(* Returns a new context and a semantically checked block item *)
and check_block_item ctx = function
    LVar(v) -> let (ctx, sv) = check_var ctx v in (ctx, SLVar(sv))
  | Expr(e) -> (ctx, SExpr(check_expr ctx e))
  | Return(e) -> (ctx, SReturn(check_expr ctx e))

(* Returns the type of the block (type of its last item) and a list of
 * semantically-checked block items *)
and check_block ctx = function
    [] -> (ScalarType TNone, [])
  | [item] ->
        let (_, item) = check_block_item ctx item in
        (match item with
            SLVar(_) | SReturn(_) -> ScalarType TNone
          | SExpr(t, _) -> t)
        , [item]
  | hd::tl ->
        let (ctx, item) = check_block_item ctx hd in
        let (t, block) = check_block ctx tl in
        (t, item::block)

(* Look for duplicates in a list of names *)
let check_dup kind where names =
    let sorted = List.sort (compare) names in
    let rec dups = function
        [] -> ()
      | (a::b::_) when a = b ->
              fail ("duplicate " ^ kind ^ " in " ^ where ^ ": " ^ a)
      | _ :: t -> dups t
    in dups sorted

(* Check bindings for duplicates and None types.
 * Returns a new context with the parameters as local variables and a list of
 * semantically checked parameters *)
let rec add_params ctx params = match params with
    [] -> ctx
  | Param(id, type_)::tl ->
        let sv = (id, type_, None) in
        let ctx = { ctx with variables = add_var ctx.variables sv } in
        add_params ctx tl

let check_params ctx params where =
    let _ = check_dup "parameter" where
        (List.map (function Param(id,_) -> id) params)
    in
    let _ = List.iter (function
        Param(id,ScalarType(TNone)) ->
            fail ("illegal " ^ id ^ " : None in " ^ where)
      | Param(id,ArrayType(TNone,_)) ->
            fail ("illegal " ^ id ^ " : None[] in " ^ where)
      | _ -> ()
    ) params in

    (add_params ctx params,
    List.map (function Param(id, type_) -> (id, type_)) params)

(* Check global program declarations: Global variable, function and template.
 * Returns a modified context and a semantically checked program declaration.
 *)
let check_pdecl ctx = function
    Func(id, type_, params, body) ->
        let (lctx, sp) = check_params ctx params id in
        (* Add itself to its local context: allow recursion *)
        let lctx = {
            lctx with functions = add_func ctx.functions (id,type_,sp,[])
        } in
        let (_, sbody) = check_block lctx body in
        let sf = (id, type_, sp, sbody) in
        ({ ctx with functions = add_func ctx.functions sf }, SFunc sf)
  | Template(_, _, _) ->
        fail "Not implemented" (* TODO
        let st = STemplate(id, check_params params, check_tblock ctx body) in
        ({ ctx with templates = add_templ ctx.templates st}, st)*)
  | GVar(v) ->
        let (ctx, sv) = check_var ctx v in
        (ctx, SGVar(sv))

let rec check_pdecls ctx = function
    [] -> []
  | hd::tl -> let (ctx, d) = check_pdecl ctx hd in d::(check_pdecls ctx tl)

let coerce_func (sf:sfunc) =
    let (id, ftype, sparams, body) = sf in

    let rec coerce_sexpr ty (t,e) = match (ty, t) with
        (ScalarType (TInt _), ScalarType TAInt)
      | (ScalarType (TFloat _), ScalarType TAFloat) ->
            (match e with
                SLInt _ | SLFloat _ ->
                    (ty, e)
              | SBinop(se1, op, se2) ->
                    (ty, SBinop (coerce_sexpr ty se1, op,
                                 coerce_sexpr ty se2))
              | SUnop(op, se) ->
                    (ty, SUnop (op, coerce_sexpr ty se))
              | SIf(pred, then_, else_) ->
                    (ty, SIf(pred, coerce_sblock ty then_,
                             coerce_sblock ty else_))
              | _ ->
                    fail ("can't coerce expression "
                          ^ (string_of_sx e) ^ " from type "
                          ^ (string_of_type t) ^ " to type "
                          ^ (string_of_type ty))
            )
      | _ -> (t, e)

    and coerce_sblock_item ty = function
        (* A variable declaration statement has None type. Its value expression
         * must be coerced to the variable's type. For example, in:
         *
         *     var x: int8 = 5 + 5;
         *
         * the expression 5 + 5 must have type int8 *)
        SLVar(id, t, Some se) ->
            SLVar(id, t, Some (coerce_sexpr t se))

        (* A return statement has None type. Its returned expression must have
         * the type that the enclosing function expects *)
      | SReturn(se) -> SReturn(coerce_sexpr ftype se)

        (* The last SExpr in a block is the block's value, so its type must
         * match the expected type ty. *)
      | SExpr(se) ->
              SExpr(coerce_sexpr ty se)

        (* TODO: should never reach this; SLVars should have a value defined
         * after semantic analysis *)
      | _ as i -> i

    and coerce_sblock ty = function
        [] -> []
        (* The last item in a block confers the block its type *)
      | [item] -> [coerce_sblock_item ty item]
      | hd::tl ->
            let hd =
               match hd with SExpr(_) -> hd | _ -> coerce_sblock_item ty hd
            in
            hd::(coerce_sblock ty tl)
    in
    SFunc(id, ftype, sparams, coerce_sblock (ScalarType TNone) body)

let rec coerce_sprog = function
    [] -> []
  | hd::tl -> (match hd with
        SFunc(sf) -> coerce_func sf
      | _ -> hd
    )::(coerce_sprog tl)

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
    ]
    and main_func =
        Func("main", ScalarType(TInt(false,32)), [], main @ [Return(LInt(0))])
    in

    let pdecls = built_in_funcs @ pdecls @ [main_func] in

    (* Build global maps *)
    let ctx = {
        variables = StringMap.empty;
        functions = StringMap.empty;
        templates = StringMap.empty;
    } in

    coerce_sprog (check_pdecls ctx pdecls)
