(* Semantic checking for the BitTwiddler compiler *)

open Ast
open Sast
open Emit

module StringMap = Map.Make(String)

type context = {
    variables : svar StringMap.t;
    functions : sfunc StringMap.t;
}

(* Shorthand *)
let fail s = raise (Failure s)

let string_of_ctx ctx =
    let string_of_sfunc (f:sfunc) = string_of_spdecl (SFunc f) in

    let fold f m = StringMap.fold (fun k v r -> k^":"^(f  v)^"\n"^r) m "" in

    "context\n\n"
    ^ "variables\n---------\n\n" ^ (fold (string_of_svar) ctx.variables)
    ^ "functions\n---------\n\n" ^ (fold (string_of_sfunc) ctx.functions)

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

let find_elem map id =
    try StringMap.find id map
    with Not_found -> fail ("undeclared identifier " ^ id)

(* Built-in functions transformations *)
let check_emit_fmt fname ctx emit_fmt =
    let emit_err id t =
        fail ("don't know how to emit " ^ id ^ ":" ^ string_of_stype t)
    in

    let fkind = (SScalar (TInt(false, 32)), SLInt(match fname with
        "emit" -> 0 | "print" -> 1 | "fatal" -> 2
      | _ -> fail ("invalid 'emit' kind: '" ^ fname ^ "'")))
    in

    (* Builds the printf-ready format string and list of variable ids *)
    let rec build_args fmt args exprs = match exprs with
        [] -> (fmt, args)
      | (STR s)::tl -> build_args (fmt ^ s) args tl
      | (VAR id)::tl -> (match find_elem ctx.variables id with
            (id, SScalar pt,_) ->
                let tfmt = (match pt with
                    TInt(true,64) -> "%lu"
                  | TInt(false,64) -> "%ld"
                  | TInt(true,_) -> "%u"
                  | TAInt | TInt(false,_) -> "%d"
                  | TAFloat | TFloat(_) -> "%f"
                  | TString -> "%s"
                  | TBool -> "%d"
                  | _ -> emit_err id (SScalar pt)
                ) in
                build_args (fmt ^ tfmt) ((SScalar pt, SId id)::args) tl
          | (id,t,_) -> emit_err id t
        )
    in

    (* Note: args is reversed here *)
    let (fmt, args) = build_args "" [] (parse_emit_fmt emit_fmt) in
    ([fkind; (SScalar TString, SLString fmt)] @ (List.rev args))

(* Type compatibility: 'abstract' types are promoted to concrete types *)
(* TODO: upcast for different integer/float sizes *)
let check_type_compat t1 t2 =
    let failure = "Incompatible types " ^ string_of_stype t1
        ^ " and " ^ string_of_stype t2
    in

    let upcast t1 t2 = match (t1,t2) with
        (TInt(_,_), TAInt) -> (t1, t1)
      | (TAInt, TInt(_,_)) -> (t2, t2)
      | (TFloat(_), TAFloat) -> (t1, t1)
      | (TAFloat, TFloat(_)) -> (t2, t2)
      | (TAInt, TAInt) -> (TInt(false,64), TInt(false,64))
      | (TAFloat, TAFloat) -> (TFloat 64, TFloat 64)
      | (_, _) when t1 = t2 -> (t1, t2)
      | _ -> fail failure
    in

    match (t1, t2) with
        (SScalar st1, SScalar st2) ->
            let (st1', st2') = upcast st1 st2 in
                (SScalar st1', SScalar st2')
      | (SArray(st1,l1), SArray(st2,l2)) ->
            let (st1', st2') = upcast st1 st2 in
                (SArray(st1',l1), SArray(st2',l2))
      | _ -> fail failure

let rec type_of_arr_lit = function
    [] -> fail "Can't determine type of empty literal array"
  | [(SScalar t,_)] -> t
  | (SScalar t,_)::tl when t = type_of_arr_lit tl -> t
  | _ -> fail "Array literal has mixed or invalid types"

(*
 * Checkers that return semantically checked elements
 *)

(* Check expression. Return sexpr. *)
let rec check_expr ctx = function
    LInt l -> (SScalar TAInt, SLInt l)
  | LFloat l -> (SScalar TAFloat, SLFloat l)
  | LString l -> (SScalar TString, SLString l)
  | LBool l -> (SScalar TBool, SLBool l)
  | LArray el ->
        (* sel = semantically-checked expression list *)
        let sel = List.map (check_expr ctx) el in
        (SArray(type_of_arr_lit sel, Some((size_t,SLInt(List.length sel)))),
         SLArray sel)
  | Id s ->
        let (_,type_,_) = find_elem ctx.variables s in (type_, SId s)

    (* A Subscr a[i] will be transformed into:
     *
     *    if i >= 0 and i < len(a) {
     *        a[i];
     *    } else {
     *        fail("array access out of bounds\n");
     *        0;
     *    }
     *
     *)
  | Binop(a, Subscr, i) ->
        let (a_t, a') = check_expr ctx a
        and (i_t, i') = check_expr ctx i in

        let failure =
            "Operator " ^ string_of_op Subscr ^ " not defined for types "
            ^ string_of_stype a_t ^ " and " ^ string_of_stype i_t
        in

        (* Check that the types are compatible *)
        let ty = match (a_t, is_integer i_t) with
            (SArray (t,_), true) -> SScalar t
          | (SScalar TString, true) -> char_t
          | _ -> fail failure
        in

        let i_t = if i_t=SScalar TAInt then size_t else i_t in

        let pty =
            match ty with SScalar t -> t | _ -> fail "internal error"
        in

        (* Generate bounds-checked array access *)
        let pred = check_expr ctx (Binop(
            Binop(i,GtEq,LInt 0),
            And,
            Binop(i,Lt,Call("len",[a]))
        )) in

        let then_ = [SExpr(ty, SBinop((a_t,a'), Subscr, (i_t,i')))] in

        let else_ = [
            SExpr(check_expr ctx (
                Call("fatal", [LString "array access out of bounds"])
            ));
            SExpr(ty, match pty with
                TInt _   -> SLInt 0
              | TFloat _ -> SLFloat 0.0
              | TString  -> SLString ""
              | _ -> fail ("internal error: don't know what to return"
                          ^ " for out of bounds access in "
                          ^ string_of_expr (Binop(a,Subscr,i)))
            )
        ] in

        (ty, SIf(pred, then_, else_))

  | Binop(e1,op,e2) ->
        let failure t1 t2 =
            "Operator " ^ string_of_op op ^ " not defined for types "
            ^ string_of_stype t1 ^ " and " ^ string_of_stype t2
        in

        let (t1, e1') = check_expr ctx e1
        and (t2, e2') = check_expr ctx e2 in

        (* Promote types and check that they are compatible *)
        let (t1', t2') = check_type_compat t1 t2 in

        let ty = match op with
            (* Overloaded Plus *)
            Plus -> (match (t1', t2') with
                (* String concatenation *)
                (SScalar TString, _) -> t1'
                (* Array concatenation *)
              | (SArray(st1',Some(_, SLInt l1)),
                 SArray(_,Some(_, SLInt l2))) ->
                    SArray(st1',Some(size_t, SLInt(l1+l2)))
                (* Number addition *)
              | (t1',_) when is_number t1' -> t1'
              | _ -> fail (failure t1' t2'))

            (* Arithmetic: defined for numbers *)
          | Minus | Times | Div when is_number t1' -> t1'
            (* Defined for Integers only *)
          | Rem | LShift | RShift | BwOr | BwAnd when is_integer t1' -> t1'
            (* Boolean operations *)
          | And | Or when is_bool t1' -> SScalar TBool
          | Lt | LtEq | Eq | NEq | GtEq | Gt -> SScalar TBool
          | Assign -> t2'
          | _ -> fail (failure t1' t2')
        in
        (ty, SBinop((t1',e1'), op, (t2',e2')))

  | Unop(uop, e) ->
        let (t, e') = check_expr ctx e in
        let ty = match (t, uop) with
            (SScalar TBool, Not) -> t
          | (SScalar (TInt _), BwNot)
          | (SScalar (TInt _), Neg)
          | (SScalar (TFloat _), Neg) -> t
          | _ -> fail ("Operator " ^ string_of_uop uop
                                   ^ " not defined for type "
                                   ^ string_of_stype t)
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
        (* conds is (expr option * stmt list) list
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
                    (SScalar TBool,_) as e' -> Some e'
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


  (* When 'emit' is called, we have to build its arguments from the format
   * string *)
  | Call(id, el) when id="emit" || id="print" || id="fatal" -> (match el with
        [(LString s)] ->
            (SScalar TNone, SCall("__bt_emit", check_emit_fmt id ctx s))
      | _ -> fail ("'" ^ id ^ "' requires a single literal string argument")
    )

  (* 'len' is expected to be applied to arrays or strings only *)
  | Call(id, el) when id="len" -> (match el with
        [e] ->
            let (t,e') = check_expr ctx e in
            let fname = match t with
                SScalar TString | SArray _ -> "__bt_len"
              | _ -> fail (id ^ " can't be applied to type: "
                           ^ string_of_stype t)
            in
            (size_t, SCall(fname, [(t,e')]))
      | _ -> fail ("len requires a single array or string argument")
    )

  | Call(id, el) ->
        let (_,type_,_,_) = find_elem ctx.functions id in
        (type_, SCall(id, List.map (check_expr ctx) el))

and check_type ctx = function
    ScalarType t -> SScalar t
  | ArrayType(t, Some e) ->
        let st, se = check_expr ctx e in
        let st = match st with
            SScalar TAInt -> size_t
          | SScalar TInt _ -> st
          | _ -> fail ("array size can't be of type " ^ string_of_stype st)
        in
        SArray(t, Some (st, se))
  | ArrayType(t, None) -> SArray(t, None)

and check_var ctx v =
    let Var(id, t, e) = v in
    let (t, e) = match (t, e) with
        (* Both type and initial value, check that types are compatible *)
        (Some t, Some e) ->
            let t = check_type ctx t in
            let (st, se) = check_expr ctx e in
            let _ = check_type_compat t st in
            (t, (st, se))

        (* Only type was declared, add automatic input reading *)
      | (Some t, None) ->
            let t = check_type ctx t in
            (t, (t, SCall("__bt_read", [])))

        (* Only value was declared, coerce type *)
      | (None, Some e) ->
            let (st, se) = check_expr ctx e in
            (match st with
                SScalar TAInt | SScalar TAFloat ->
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
and check_stmt ctx = function
    LVar v -> let (ctx, sv) = check_var ctx v in (ctx, SLVar sv)
  | Expr e -> (ctx, SExpr(check_expr ctx e))
  | Return e -> (ctx, SReturn(check_expr ctx e))
  | While(pred, block) ->
        let pred' = match (check_expr ctx pred) with
            (SScalar TBool, _) as pred' -> pred'
          | (_,sx) -> fail ("Non-boolean expression in while predicate: "
                            ^ (string_of_sx sx))
        in

        let (_, block') = check_block ctx block in
        (ctx, SWhile(pred', block'))

  | For(idx, item, e, block) ->
        let (t, e') = check_expr ctx e in
        let idx_t = size_t in
        let item_t = match t with
            SArray(TString,_) -> fail "Can't iterate over array of strings"
          | SArray(pt,_) -> SScalar pt
          | SScalar TString -> char_t
          | _ -> fail ("Can't iterate over expression of type "
                       ^ string_of_stype t)
        in
        let idx_svar = (idx, idx_t, Some (idx_t, SLInt(0))) in
        let item_svar = (item, item_t, None) in
        let lvars = List.fold_left (
            fun vars sv -> add_var vars sv
        ) ctx.variables [idx_svar; item_svar] in
        let lctx = { ctx with variables = lvars } in
        let (_, block') = check_block lctx block in
        (ctx, SFor(idx_svar, item_svar, (t, e'), block'))


(* Returns the type of the block (type of its last item) and a list of
 * semantically-checked block items *)
and check_block ctx = function
    [] -> (SScalar TNone, [])
  | [item] ->
        let (_, item) = check_stmt ctx item in
        (match item with
            SLVar _ | SReturn _ | SWhile _ | SFor _ -> SScalar TNone
          | SExpr(t, _) -> t)
        , [item]
  | hd::tl ->
        let (ctx, item) = check_stmt ctx hd in
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
        let sv = (id, check_type ctx type_, None) in
        let ctx = { ctx with variables = add_var ctx.variables sv } in
        add_params ctx tl

let check_params ctx params where =
    let _ = check_dup "parameter" where
        (List.map (function Param(id,_) -> id) params)
    in
    let _ = List.iter (function
        Param(id,ScalarType TNone) ->
            fail ("illegal " ^ id ^ " : None in " ^ where)
      | Param(id,ArrayType(TNone,_)) ->
            fail ("illegal " ^ id ^ " : None[] in " ^ where)
      | _ -> ()
    ) params in

    (add_params ctx params,
    List.map (function Param(id, type_) -> (id, check_type ctx type_)) params)

(* Check global program declarations: Global variable and function
 * Returns a modified context and a semantically checked program declaration.
 *)
let check_pdecl ctx = function
    Func(id, type_, params, body) ->
        let (lctx, sp) = check_params ctx params id in
        let stype = check_type ctx type_ in
        (* Add itself to its local context: allow recursion *)
        let lctx = {
            lctx with functions = add_func ctx.functions (id,stype,sp,[])
        } in
        let (_, sbody) = check_block lctx body in
        let sf = (id, stype, sp, sbody) in
        ({ ctx with functions = add_func ctx.functions sf }, SFunc sf)
  | GVar(v) ->
        let (ctx, sv) = check_var ctx v in
        (ctx, SGVar(sv))

let rec check_pdecls ctx = function
    [] -> []
  | hd::tl -> let (ctx, d) = check_pdecl ctx hd in d::(check_pdecls ctx tl)

let coerce_func (sf:sfunc) =
    let (id, ftype, sparams, body) = sf in

    let coerce_fail e t ty =
        fail ("can't coerce expression " ^ (string_of_sx e) ^ " from type "
              ^ (string_of_stype t) ^ " to type " ^ (string_of_stype ty))
    in

    let rec coerce_sexpr ty (t,e) = match (ty, t) with
        (t1, t2) when t1=t2 -> (t, e)
      | (SScalar (TInt _), SScalar TAInt)
      | (SScalar (TFloat _), SScalar TAFloat) ->
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
              | _ -> coerce_fail e t ty
            )
      | (SArray(et, _), _) ->
            (match e with
                SLArray el -> (ty,
                    SLArray(List.map (coerce_sexpr (SScalar et)) el)
                )
              | _ -> coerce_fail e t ty
            )
      | _ -> (t, e)

    and coerce_sstmt ty = function
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
      | SReturn se -> SReturn(coerce_sexpr ftype se)

        (* The last SExpr in a block is the block's value, so its type must
         * match the expected type ty. *)
      | SExpr se -> SExpr(coerce_sexpr ty se)

        (* TODO: should never reach this; SLVars should have a value defined
         * after semantic analysis *)
      | _ as i -> i

    and coerce_sblock ty = function
        [] -> []
        (* The last item in a block confers the block its type *)
      | [item] -> [coerce_sstmt ty item]
      | hd::tl ->
            let hd =
               match hd with SExpr _ -> hd | _ -> coerce_sstmt ty hd
            in
            hd::(coerce_sblock ty tl)
    in
    SFunc(id, ftype, sparams, coerce_sblock (SScalar TNone) body)

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
        Func("emit", ScalarType TNone, [], []);
        Func("len", ScalarType(TInt(true,64)), [], []);
    ]
    and main_func =
        Func("main", ScalarType(TInt(false,32)), [], main @ [Return(LInt 0)])
    in

    let pdecls = built_in_funcs @ pdecls @ [main_func] in

    (* Build global maps *)
    let ctx = {
        variables = StringMap.empty;
        functions = StringMap.empty;
    } in

    coerce_sprog (check_pdecls ctx pdecls)
