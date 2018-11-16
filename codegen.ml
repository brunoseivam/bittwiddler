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
    and i32_t  = L.i32_type context in

    (* Built-ins *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in

    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module
    in

    let build_main_body main_lines =
        let main_t = L.function_type i32_t [| |] in
        let the_function = L.define_function "main" main_t the_module in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* TODO: this newline shouldn't be here. Newlines specified in .bt
         * source files are not escaped! *)
        let str_fmt = L.build_global_stringptr "%s\n" "fmt" builder in

        (* Expression builder *)
        let rec expr builder e = match e with
            A.LString(s) -> L.build_global_stringptr s "" builder
          | A.Call("emit", [ex]) ->
                  L.build_call printf_func [| str_fmt; (expr builder ex) |]
                    "printf" builder
          | _ -> raise (Failure ("not implemented: " ^ A.string_of_expr e))
        in

        let add_terminal builder instr =
            match L.block_terminator (L.insertion_block builder) with
                Some _ -> ()
              | None   -> ignore (instr builder)
        in

        let block builder = function
            A.Expr(e) -> ignore(expr builder e); builder
          | _ -> raise (Failure ("block builder not implemented"))
        in

        let builder = List.fold_left block builder main_lines in
        add_terminal builder (L.build_ret (L.const_int i32_t 0))
    in

    match prog with SProgram(_, main_lines) ->
        build_main_body main_lines;

    the_module
