(* Code generation: translate a semantically checked AST into LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* TODO: maybe we only need to store llvalues here *)
type ctx = {
    vars : (L.llvalue * svar) StringMap.t list;     (* stack of vars *)
    funcs : (L.llvalue * sfunc) StringMap.t;        (* functions *)
    templs : (L.llvalue * stempl) StringMap.t;      (* templates *)
    cur_func : L.llvalue option;                    (* current function *)
}

let string_of_ctx name ctx =
    let string_of_var k (lv,_) =
        "var " ^ k ^ ": " ^ (L.string_of_llvalue lv)
    and string_of_func k (lv,_) =
        "func " ^ k ^ ": " ^ (L.string_of_llvalue lv)
    in

    let string_of_vars m i =
        StringMap.fold (fun k v r -> i^(string_of_var k v)^"\n"^r) m ""
    and string_of_funcs m i =
        StringMap.fold (fun k v r -> i^(string_of_func k v)^"\n"^r) m ""
    in

    let string_of_stack l i =
        List.fold_left
            (fun r v -> i ^ "[" ^ (string_of_vars v i) ^ "]\n" ^ r)
        "" l
    in

      name ^ " ctx {\n"
    ^ "  vars: {\n" ^ (string_of_stack ctx.vars "    ") ^ "  }\n"
    ^ "  funcs: {\n" ^ (string_of_funcs ctx.funcs "  ") ^ "  }\n"
    ^ "}"

(* Lookup a var in a chain of StringMaps *)
let rec chain_lookup k = function
    [] -> raise Not_found
  | m::tl -> try StringMap.find k m with Not_found -> chain_lookup k tl

let lookup_var k ctx =
    try chain_lookup k ctx.vars
    with Not_found -> raise (Failure ("variable " ^ k ^ " not found"))

(* Adds a var to the top var map of the context ctx *)
let add_var ctx (id:string) (lv:L.llvalue) (sv:svar) =
    match ctx.vars with
        [] -> raise (Failure "internal error: add_var on non-existent map")
      | hd::tl ->
            let ctx = { ctx with vars = (StringMap.add id (lv, sv) hd)::tl } in
            ctx



let translate prog =
    let context = L.global_context () in

    (* LLVM compilation module *)
    let the_module = L.create_module context "BitTwiddler" in

    (* Get types from context *)
    let i8_t   = L.i8_type context
    (*and i16_t  = L.i16_type context*)
    and i32_t  = L.i32_type context
    (*and i64_t  = L.i64_type context*)
    and f32_t  = L.float_type context
    and f64_t  = L.double_type context
    and void_t = L.void_type context in

    (* Get LLVM type from BitTwiddler type *)
    let ltype_of_type = function
        A.ScalarType(t) -> (match t with
            A.TAInt -> i32_t
          | A.TInt(_,w) -> L.integer_type context w
          | A.TFloat(32) -> f32_t
          | A.TAFloat | A.TFloat(64) -> f64_t
          | A.TNone -> void_t
          | _ -> raise (Failure ("type not implemented " ^ A.string_of_ptype t))
        )
      | _ as t -> raise (Failure ("type not implemented " ^ A.string_of_type t))
    in

    (* Create an alloca instruction in the entry block of the function *)
    let create_entry_block_alloca the_function id type_ =
        let builder = L.builder_at context (
            L.instr_begin (L.entry_block the_function)
        ) in
        L.build_alloca (ltype_of_type type_) id builder
    in

    (* Compare to zero *)
    let build_is_nonzero v builder =
        let zero = L.const_int (L.type_of v) 0 in
        L.build_icmp L.Icmp.Ne v zero "tmp" builder
    in

    (* Casts LLVM bool (i1) to BitTwiddler's boolean (uint8) *)
    let build_cast_bool v builder =
        L.build_zext v (ltype_of_type A.boolean) "tmp" builder
    in

    (* Built-ins *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in

    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module
    in

    (* Expression builder *)
    let rec build_expr ctx builder = function
        (t, SLInt i) ->
            L.const_int (ltype_of_type t) i
      | (t, SLFloat f) ->
            L.const_float (ltype_of_type t) f
      | (_, SLString s) ->
            L.build_global_stringptr s "" builder
      | (_, SLArray _) ->
            raise (Failure "arrays not implemented yet")
      | (A.ScalarType _, SId id) ->
            let (v, _) = lookup_var id ctx in
            L.build_load v id builder
      | (_, SId _) ->
            raise (Failure "arrays not implemented yet")

        (* Assignment *)
      | (A.ScalarType _, SBinop ((_, SId id), A.Assign, e)) ->
            let e' = build_expr ctx builder e in
            let (v,_) = lookup_var id ctx in
            ignore(L.build_store e' v builder); e'

        (* Binary operation returning scalar *)
      | (A.ScalarType t, SBinop (e1, op, e2)) -> (match t with
            A.TAInt -> raise (Failure "internal error: got abstract int")
          | A.TAFloat -> raise (Failure "internal error: got abstract float")
          | A.TInt(u,_) ->
                let e1' = build_expr ctx builder e1
                and e2' = build_expr ctx builder e2 in
                let r = (match op with
                  A.Plus    -> L.build_add
                | A.Minus   -> L.build_sub
                | A.Times   -> L.build_mul
                | A.Div     -> if u then L.build_udiv else L.build_sdiv
                | A.Rem     -> if u then L.build_urem else L.build_srem
                | A.LShift  -> L.build_shl
                | A.RShift  -> if u then L.build_lshr else L.build_ashr
                | A.BwOr  | A.Or  -> L.build_or
                | A.BwAnd | A.And -> L.build_and
                | A.Lt      -> L.build_icmp (if u then L.Icmp.Ult else L.Icmp.Slt)
                | A.LtEq    -> L.build_icmp (if u then L.Icmp.Ule else L.Icmp.Sle)
                | A.Eq      -> L.build_icmp L.Icmp.Eq
                | A.NEq     -> L.build_icmp L.Icmp.Ne
                | A.GtEq    -> L.build_icmp (if u then L.Icmp.Uge else L.Icmp.Sge)
                | A.Gt      -> L.build_icmp (if u then L.Icmp.Ugt else L.Icmp.Sgt)
                | _ -> raise (Failure ("internal error: operation "
                                       ^ A.string_of_op op
                                       ^ " not implemented for integers"))
                ) e1' e2' "tmp" builder in

                (* Boolean operations can return:
                 *   - and/or: the type of the operands
                 *   - comparison: i1
                 * But we must ensure that it returns BitTwiddler's definition
                 * of a boolean, uint8.
                 *)
                (match op with
                    A.And | A.Or ->
                        let b = build_is_nonzero r builder in
                        build_cast_bool b builder
                  | A.Lt | A.LtEq | A.Eq | A.NEq | A.GtEq | A.Gt ->
                        build_cast_bool r builder
                  | _ -> r
                )
          | A.TFloat(_) ->
                let e1' = build_expr ctx builder e1
                and e2' = build_expr ctx builder e2 in
                (match op with
                  A.Plus    -> L.build_fadd
                | A.Minus   -> L.build_fsub
                | A.Times   -> L.build_fmul
                | A.Div     -> L.build_fdiv
                | A.Lt      -> L.build_fcmp L.Fcmp.Olt
                | A.LtEq    -> L.build_fcmp L.Fcmp.Ole
                | A.Eq      -> L.build_fcmp L.Fcmp.Oeq
                | A.NEq     -> L.build_fcmp L.Fcmp.One
                | A.GtEq    -> L.build_fcmp L.Fcmp.Oge
                | A.Gt      -> L.build_fcmp L.Fcmp.Ogt
                | _ -> raise (Failure ("internal error: operation "
                                       ^ A.string_of_op op
                                       ^ " not implemented for floats"))
                ) e1' e2' "tmp" builder
           | _ -> raise (Failure ("internal error: operation "
                                  ^ A.string_of_op op
                                  ^ " not implemented for type "
                                  ^ A.string_of_ptype t))
        )
        (* Unary operation on integer *)
      | (A.ScalarType (A.TInt _), SUnop (op, e)) ->
            let e' = build_expr ctx builder e in
            (match op with
                A.BwNot -> L.build_not e' "tmp" builder
              | A.Not ->
                    let b = build_is_nonzero e' builder in
                    let n = L.build_not b "tmp" builder in
                    build_cast_bool n builder
              | A.Neg -> L.build_neg e' "tmp" builder)

        (* Unary operation on float *)
      | (A.ScalarType (A.TFloat _), SUnop (A.Neg, e)) ->
            L.build_fneg (build_expr ctx builder e) "tmp" builder

      | (_, SCall("emit", args)) ->
            L.build_call
                printf_func
                (Array.of_list (List.map (build_expr ctx builder) args))
                "printf"
                builder
      | _ as e -> raise (Failure ("expr not implemented: " ^ string_of_sexpr e))

    (*
     * Build an sblock_item
     *)
    and build_block_item ctx builder = function
        SLVar(_, id, type_, Some e as v) ->
            (match ctx.cur_func with
                (* Allocate var on the stack and add it to the context *)
                Some f ->
                    let lv = create_entry_block_alloca f id type_ in
                    let ctx = add_var ctx id lv v in
                    let e' = build_expr ctx builder e in
                    ignore(L.build_store e' lv builder); (ctx, builder)
              | None ->
                    raise (Failure "internal error: local var without function")
            )
      | SExpr(e) -> ignore(build_expr ctx builder e); (ctx, builder)
      | SReturn(e) -> ignore(build_expr ctx builder e); (ctx, builder)
      | _ as i -> raise (Failure ("can't build block item "
                                  ^ (string_of_sblock_item i)))

    (*
     * Build a block
     *)
    and build_block ctx builder = function
        [] ->
            builder
      | hd::tl ->
            let (ctx, builder) = build_block_item ctx builder hd in
            build_block ctx builder tl
    in

    (*
     * Build a sprogram_decl
     *)

    (* Helper: list of parameters -> array of ltypes *)
    let ltypes_of_params params = Array.of_list (
        List.map (fun (_, t) -> ltype_of_type t) params
    ) in

    (* Helper: 'LLVM return' from function type *)
    let ret_of_type t = match t with
        A.ScalarType pt -> (match pt with
            A.TAInt | A.TInt(_,_) ->
                L.build_ret (L.const_int (ltype_of_type t) 0)
          | A.TAFloat | A.TFloat(_) ->
                L.build_ret (L.const_float (ltype_of_type t) 0.0)
          | A.TNone ->
                L.build_ret_void
          | _ -> raise (Failure ("ret type not implemented " ^ A.string_of_ptype pt))
        )
      | _ -> raise (Failure ("ret type not implemented " ^ A.string_of_type t))
    in

    (* Build a global variable *)
    let build_gvar ctx v =
        let (_, id, type_, _) = v in
        let ltype = ltype_of_type type_ in
        let init = match type_ with
            A.ScalarType(A.TInt(_,_)) -> L.const_int ltype 0
          | A.ScalarType(A.TFloat(_)) -> L.const_float ltype 0.0
          | _ -> raise (Failure ("gvar init " ^ (A.string_of_type type_)
                                 ^ "not implemented"))
        in
        let the_var = L.define_global id init the_module in
        add_var ctx id the_var v
    in

    (* Build a function *)
    let build_func ctx f =
        let (id, type_, params, body) = f in
        let params_ltypes = ltypes_of_params params in
        let ftype = L.function_type (ltype_of_type type_) params_ltypes in
        let the_function = L.define_function id ftype the_module in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Add a parameter to the stack *)
        let add_param ctx id type_ =
            let sv = (false, id, type_, None) in
            let lv = create_entry_block_alloca the_function id type_ in
            add_var ctx id lv sv
        in

        let rec add_params ctx = function
            [] -> ctx
          | (id,type_)::tl ->
                let ctx = add_param ctx id type_ in
                add_params ctx tl
        in

        let add_terminal builder instr =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None   -> ignore (instr builder)
        in

        (* Allocate parameters on the stack and assign passed in values *)
        let lctx = add_params ctx params in
        let _ = List.iter2
            (fun (id,_) value ->
                ignore(L.build_store value (fst (lookup_var id lctx)) builder))
            params
            (Array.to_list (L.params the_function))
        in

        (* Build the body of the function *)
        let lctx = { ctx with
            cur_func = Some the_function;
            vars = StringMap.empty::ctx.vars;
        } in
        let builder = build_block lctx builder body in
        let _ = add_terminal builder (ret_of_type type_) in

        (* Returns a context with this function added to it *)
        { ctx with funcs = StringMap.add id (the_function, f) ctx.funcs }
    in

    (* Build a template *
    let build_templ ctx t =
        raise (Failure "templates: not implemented yet")
    in*)

    (* Build a program declaration *)
    let build_pdecl ctx = function
        SGVar(v) -> build_gvar ctx v
      | SFunc(f) -> build_func ctx f
      | STemplate(_) -> raise (Failure "templates not implemented yet")
    in

    (* Build all program declarations *)
    let rec build_pdecls ctx = function
        [] -> ()
      | hd::tl ->
            let ctx = build_pdecl ctx hd in
            build_pdecls ctx tl
    in

    let global_ctx = {
        vars = [StringMap.empty];
        funcs = StringMap.empty;
        templs = StringMap.empty;
        cur_func = None;
    } in

    build_pdecls global_ctx prog;
    the_module

