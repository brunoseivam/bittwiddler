(* Code generation: translate a semantically checked AST into LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

type ctx = {
    vars : L.llvalue StringMap.t list;  (* stack of vars *)
    funcs : L.llvalue StringMap.t;      (* functions *)
    templs : L.llvalue StringMap.t;     (* templates *)
    cur_func : L.llvalue option;        (* current function *)
}

let string_of_ctx name ctx =
    let string_of_var k lv =
        "var " ^ k ^ ": " ^ (L.string_of_llvalue lv)
    and string_of_func k lv =
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
let add_var ctx (id:string) (lv:L.llvalue) =
    match ctx.vars with
        [] -> raise (Failure "internal error: add_var on non-existent map")
      | hd::tl ->
            let ctx = { ctx with vars = (StringMap.add id lv hd)::tl } in
            ctx

let translate prog =
    let context = L.global_context () in

    (* LLVM compilation module *)
    let the_module = L.create_module context "BitTwiddler" in

    (* Get types from context *)
    let i1_t   = L.i1_type context
    and i8_t   = L.i8_type context
    and i16_t  = L.i16_type context
    and i32_t  = L.i32_type context
    and i64_t  = L.i64_type context
    and f32_t  = L.float_type context
    and f64_t  = L.double_type context
    and void_t = L.void_type context in

    let __bt_arr_t = L.struct_type context [|
        i64_t; i64_t; L.pointer_type i8_t
    |] in

    let __bt_str_t = L.struct_type context [|
        i64_t; L.pointer_type __bt_arr_t
    |] in


    (* Get LLVM type from BitTwiddler type *)
    let ltype_of_type = function
        A.ScalarType(t) -> (match t with
            A.TAInt -> i32_t
          | A.TInt(_,w) -> L.integer_type context w
          | A.TFloat(32) -> f32_t
          | A.TAFloat | A.TFloat(64) -> f64_t
          | A.TBool -> i1_t
          | A.TNone -> void_t
          | A.TString -> L.pointer_type __bt_str_t
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

    (* Add a terminal to a block *)
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
            Some _ -> ()
          | None   -> ignore (instr builder)
    in

    (* Compare to zero *)
    let build_is_nonzero v builder =
        let zero = L.const_int (L.type_of v) 0 in
        L.build_icmp L.Icmp.Ne v zero "tmp" builder
    in

    (* Runtime functions *)
    let __bt_emit : L.llvalue =
        let __bt_emit_t =
            L.var_arg_function_type void_t [| i32_t; L.pointer_type i8_t |]
        in
        L.declare_function "__bt_emit" __bt_emit_t the_module
    in

    let __bt_read n t =
        let ftype = L.function_type t [| |] in
        L.declare_function ("__bt_read_" ^ n) ftype the_module
    in

    let __bt_read_i8  = __bt_read "i8"  i8_t in
    let __bt_read_i16 = __bt_read "i16" i16_t in
    let __bt_read_i32 = __bt_read "i32" i32_t in
    let __bt_read_i64 = __bt_read "i64" i64_t in
    let __bt_read_f32 = __bt_read "f32" f32_t in
    let __bt_read_f64 = __bt_read "f64" f64_t in

    let __bt_read_str =
        let ftype = L.function_type (L.pointer_type __bt_str_t) [| |] in
        L.declare_function "__bt_str_read" ftype the_module
    in

    let __bt_arr_new =
        let __bt_arr_new_t =
            L.function_type (L.pointer_type __bt_arr_t) [|
                i64_t; i64_t
            |]
        in
        L.declare_function "__bt_arr_new" __bt_arr_new_t the_module
    in

    let __bt_str_new =
        let __bt_str_new_t =
            L.function_type (L.pointer_type __bt_str_t) [|
                L.pointer_type i8_t
            |]
        in
        L.declare_function "__bt_str_new" __bt_str_new_t the_module
    in

    let __bt_str_concat =
        let __bt_str_concat_t =
            L.function_type (L.pointer_type __bt_str_t) [|
                L.pointer_type __bt_str_t; L.pointer_type __bt_str_t;
            |]
        in
        L.declare_function "__bt_str_concat" __bt_str_concat_t the_module
    in

    let __bt_str_get =
        let __bt_str_get_t =
            L.function_type (L.pointer_type i8_t) [|
                L.pointer_type __bt_str_t;
            |]
        in
        L.declare_function "__bt_str_get" __bt_str_get_t the_module
    in

    (* Expression builder *)
    let rec build_expr ctx builder = function
        (t, SLInt i) ->
            (builder, L.const_int (ltype_of_type t) i)
      | (t, SLFloat f) ->
            (builder, L.const_float (ltype_of_type t) f)
      | (_, SLString s) ->
            let gptr = L.build_global_stringptr s "" builder in
            let s =
                L.build_call __bt_str_new [| gptr |] "__bt_str_new" builder
            in
            (builder, s)
      | (_, SLBool b) ->
            (builder, L.const_int i1_t (if b then 1 else 0))
      | (_, SLArray _) ->
            raise (Failure "arrays not implemented yet")
      | (A.ScalarType _, SId id) ->
            (builder, L.build_load (lookup_var id ctx) id builder)
      | (_, SId _) ->
            raise (Failure "arrays not implemented yet")

        (* Assignment *)
      | (A.ScalarType _, SBinop ((_, SId id), A.Assign, e)) ->
            let (builder, e') = build_expr ctx builder e in
            ignore(L.build_store e' (lookup_var id ctx) builder);
            (builder, e')

        (* Binary operation on integers *)
      | (_, SBinop ((A.ScalarType A.TInt (u,_),_) as e1, op, e2)) ->
            let (_,e1') = build_expr ctx builder e1
            and (_,e2') = build_expr ctx builder e2 in
            let r = (match op with
                A.Plus    -> L.build_add
              | A.Minus   -> L.build_sub
              | A.Times   -> L.build_mul
              | A.Div     -> if u then L.build_udiv else L.build_sdiv
              | A.Rem     -> if u then L.build_urem else L.build_srem
              | A.LShift  -> L.build_shl
              | A.RShift  -> if u then L.build_lshr else L.build_ashr
              | A.BwOr    -> L.build_or
              | A.BwAnd   -> L.build_and
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
            (builder, match op with
                A.And | A.Or -> build_is_nonzero r builder
              | _ -> r)

        (* Binary operation on floats *)
      | (_, SBinop ((A.ScalarType A.TFloat _, _) as e1, op, e2)) ->
            let (_,e1') = build_expr ctx builder e1
            and (_,e2') = build_expr ctx builder e2 in
            let r = (match op with
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
            ) e1' e2' "tmp" builder in
            (builder, r)

        (* Binary operation on bools *)
      | (_, SBinop ((A.ScalarType A.TBool,_) as e1, op, e2)) ->
            let (_,e1') = build_expr ctx builder e1
            and (_,e2') = build_expr ctx builder e2 in
            let r = (match op with
                  A.Or  -> L.build_or
                | A.And -> L.build_and
                | _ -> raise (Failure ("internal error: operation "
                                       ^ A.string_of_op op
                                       ^ " not implemented for bools"))
            ) e1' e2' "tmp" builder in
            (builder, r)

        (* Unary operation on integer *)
      | (A.ScalarType (A.TInt _), SUnop (uop, e)) ->
            let (builder, e') = build_expr ctx builder e in
            (builder, match uop with
                A.BwNot -> L.build_not e' "tmp" builder
              | A.Neg -> L.build_neg e' "tmp" builder
              | _ -> raise (Failure ("internal error: operation "
                                     ^ A.string_of_uop uop
                                     ^ " not implemented for integers")))

        (* Unary operation on float *)
      | (A.ScalarType (A.TFloat _), SUnop (A.Neg, e)) ->
            let (builder, e') = build_expr ctx builder e in
            (builder, L.build_fneg e' "tmp" builder)

        (* Unary operation on bool *)
      | (A.ScalarType A.TBool, SUnop (A.Not, e)) ->
            let (builder, e') = build_expr ctx builder e in
            (builder, L.build_not e' "not" builder)

        (* Conditional block (it's an expression) *)
      | (t, SIf (pred, then_, else_)) ->
            let the_function = match ctx.cur_func with
                Some f -> f
              | None -> raise (Failure "internal error: no current function")
            in

            (* The result of the block expression will be stored here *)
            let block_res = match t with
                A.ScalarType A.TNone -> None
              | A.ScalarType _ ->
                      Some (create_entry_block_alloca the_function "blres" t)
              | A.ArrayType _ -> raise (Failure "arrays not implemented yet")
            in

            (* Build predicate *)
            let (_,pred') = build_expr ctx builder pred in
            let merge_bb = L.append_block context "merge" the_function in
            let build_br_merge = L.build_br merge_bb in (* partial function *)

            (* Build then block *)
            let then_bb = L.append_block context "then" the_function in
            let then_builder = (L.builder_at_end context then_bb) in
            let then_builder = (build_block ctx then_builder block_res then_) in
            add_terminal then_builder build_br_merge;

            (* Build else block *)
            let else_bb = L.append_block context "else" the_function in
            let else_builder = (L.builder_at_end context else_bb) in
            let else_builder = (build_block ctx else_builder block_res else_) in
            add_terminal else_builder build_br_merge;

            let _ = L.build_cond_br pred' then_bb else_bb builder in
            let _ = L.move_block_after else_bb merge_bb in
            let builder = L.builder_at_end context merge_bb in
            (builder, match block_res with
                Some v -> L.build_load v "res" builder
              | None -> L.undef void_t)

      | (_, SCall("__bt_emit", args)) ->
            let args' = List.map (build_expr_s ctx builder) args in
            let args' = Array.of_list (List.map snd args') in
            (builder, L.build_call __bt_emit args' "" builder)

      | (A.ScalarType t, SCall("__bt_read", _)) ->
            let f = match t with
                A.TInt(_,8)  -> __bt_read_i8
              | A.TInt(_,16) -> __bt_read_i16
              | A.TInt(_,32) -> __bt_read_i32
              | A.TInt(_,64) -> __bt_read_i64
              | A.TFloat 32  -> __bt_read_f32
              | A.TFloat 64  -> __bt_read_f64
              | A.TString    -> __bt_read_str
              | _ -> raise (Failure ("automatic reading of scalar type "
                                     ^ (A.string_of_ptype t)
                                     ^ " not implemented"))
            in
            (builder, L.build_call f [| |] "__bt_read" builder)

      | (_, SCall(fname, args)) ->
            let args' = List.map (build_expr ctx builder) args in
            let args' = Array.of_list (List.map snd args') in

            let call =
                let lf =
                    try StringMap.find fname ctx.funcs
                    with Not_found ->
                        raise (Failure ("function " ^ fname ^ " not found"))
                in
                let result = if L.type_of lf = void_t then ""
                    else fname ^ "_result"
                in
                L.build_call lf args' result builder
            in
            (builder, call)

      | _ as e -> raise (Failure ("expr not implemented: " ^ string_of_sexpr e))

    (* Build an expression, flattening strings *)
    and build_expr_s ctx builder = function
        (A.ScalarType A.TString, _) as e ->
            let (builder, e') = build_expr ctx builder e in
            (builder, L.build_call __bt_str_get [| e' |] "__bt_str_get" builder)
      | _ as e -> build_expr ctx builder e

    (*
     * Build an sblock_item
     *)
    and build_block_item ctx builder = function
        SLVar(_, id, type_, Some e) ->
            (match ctx.cur_func with
                (* Allocate var on the stack and add it to the context *)
                Some f ->
                    let lv = create_entry_block_alloca f id type_ in
                    let ctx = add_var ctx id lv in
                    let (builder, e') = build_expr ctx builder e in
                    ignore(L.build_store e' lv builder);
                    (None, ctx, builder)
              | None ->
                    raise (Failure "internal error: local var without stack")
            )
      | SExpr(e) ->
            let (builder, e') = build_expr ctx builder e in
            let lval = match e with
                (A.ScalarType A.TNone, _) -> None
              | _ -> Some e'
            in
            (lval, ctx, builder)
      | SReturn(e) ->
              let (builder, e') = build_expr ctx builder e in
              ignore(L.build_ret e' builder);
              (None, ctx, builder)
      | _ as i ->
              raise (Failure ("can't build block item "
                              ^ (string_of_sblock_item i)))

    (*
     * Build a block
     *)
    and build_block ctx builder res = function
        [] ->
            builder
      | [item] ->
            (* Store the value of the last item in res *)
            let (lval, _, builder) = build_block_item ctx builder item in
            let _ = match (res, lval) with
                (Some r, Some v) -> ignore(L.build_store v r builder)
              | (None, None) -> ()
              | (Some _, None) -> raise (Failure ("Block expected to end with "
                                                  ^ "an expression"))
              | (None, Some _) -> raise (Failure ("Block expected to be of "
                                                  ^ "None type"))
            in
            builder
      | hd::tl ->
            let (_, ctx, builder) = build_block_item ctx builder hd in
            build_block ctx builder res tl
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
        add_var ctx id the_var
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
            let lv = create_entry_block_alloca the_function id type_ in
            add_var ctx id lv
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
                ignore(L.build_store value (lookup_var id lctx) builder))
            params
            (Array.to_list (L.params the_function))
        in

        (* Create function's local context *)
        let lctx = { lctx with
            funcs = StringMap.add id the_function lctx.funcs;
            cur_func = Some the_function;
            vars = StringMap.empty::lctx.vars;
        } in

        (* Build the body of the function *)
        let builder = build_block lctx builder None body in
        let _ = add_terminal builder (ret_of_type type_) in

        (* Returns a context with this function added to it *)
        { ctx with funcs = StringMap.add id the_function ctx.funcs }
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

