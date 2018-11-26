(* Code generation: translate a semantically checked AST into LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)


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
          | A.TInt(_,w,_) -> L.integer_type context w
          | A.TFloat(32) -> f32_t
          | A.TAFloat | A.TFloat(64) -> f64_t
          | A.TNone -> void_t
          | _ -> raise (Failure ("type not implemented " ^ A.string_of_ptype t))
        )
      | _ as t -> raise (Failure ("type not implemented " ^ A.string_of_type t))
    in

    let ret_of_type t = match t with
        A.ScalarType pt -> (match pt with
            A.TAInt | A.TInt(_,_,_) ->
                L.build_ret (L.const_int (ltype_of_type t) 0)
          | A.TAFloat | A.TFloat(_) ->
                L.build_ret (L.const_float (ltype_of_type t) 0.0)
          | A.TNone ->
                L.build_ret_void
          | _ -> raise (Failure ("ret type not implemented " ^ A.string_of_ptype pt))
        )
      | _ -> raise (Failure ("ret type not implemented " ^ A.string_of_type t))
    in

    (* Built-ins *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in

    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module
    in

    (* Function builder *)
    let build_func id type_ params body =
        let ftype = L.function_type (ltype_of_type type_) [| |] in
        let the_function = L.define_function id ftype the_module in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Expression builder *)
        let rec expr builder e = match e with
            (t, SLInt i) ->
                L.const_int (ltype_of_type t) i
          | (t, SLFloat f) ->
                L.const_float (ltype_of_type t) f
          | (_, SLString(s)) ->
                L.build_global_stringptr s "" builder
          | (A.ScalarType t, SBinop (e1, op, e2)) -> (match t with
                A.TAInt -> raise (Failure "internal error: got abstract int")
              | A.TAFloat -> raise (Failure "internal error: got abstract float")
              | A.TInt(u,_,_) ->
                    let u = u="u"
                    and e1' = expr builder e1
                    and e2' = expr builder e2 in
                    (match op with
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
                    ) e1' e2' "tmp" builder
              | A.TFloat(_) ->
                    let e1' = expr builder e1
                    and e2' = expr builder e2 in
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
          | (_, SCall("emit", args)) ->
                L.build_call
                    printf_func
                    (Array.of_list (List.map (expr builder) args))
                    "printf"
                    builder
          | _ -> raise (Failure ("expr not implemented: " ^ string_of_sexpr e))
        in

        let add_terminal builder instr =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None   -> ignore (instr builder)
        in

        let block builder = function
            SLVar(_) -> raise (Failure ("local var not implemented"))
          | SExpr(e) -> ignore(expr builder e); builder
          | SReturn(e) -> ignore(expr builder e); builder
        in

        let builder = List.fold_left block builder body in
        add_terminal builder (ret_of_type type_)
    in

    let SProgram(pdecls) = prog in

    let build_decl = function
        SFunc(id, type_, params, body) -> build_func id type_ params body
      | _ -> raise (Failure ("build_decl not implemented for this type"))
    in

    List.iter build_decl pdecls;
    the_module
