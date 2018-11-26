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
          | (_, SCall("emit", args)) ->
                L.build_call
                    printf_func
                    (Array.of_list (List.map (expr builder) args))
                    "printf"
                    builder
          (*| _ -> raise (Failure ("not implemented: " ^ A.string_of_expr e))*)
          | _ -> raise (Failure "expr not implemented")
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
