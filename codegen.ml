(* Code generation: translate a semantically checked AST into LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (prog) =
    let context = L.global_context () in

    (* LLVM compilation module *)
    let the_module = L.create_module context "BitTwiddler" in

    (* Get types from context *)
    let void_t = L.void_type context
    and i8_t   = L.i8_type context in

    (* Built-ins *)
    let printf_t : L.lltype =
        L.function_type void_t [| L.pointer_type i8_t |]
    in

    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module
    in

    (* Parse body builder *)
    let build_parse_body parse_lines =
        let the_function = L.define_function "parse" void_t the_module in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Expression builder *)
        let rec expr builder e = match e with
            A.LString(s) -> L.build_global_stringptr s "globalstr" builder
          | A.Call(A.Id("emit"), [ex]) ->
                L.build_call printf_func [| expr builder ex |] "printf" builder
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

        let builder = List.fold_left block builder parse_lines in
        add_terminal builder L.build_ret_void
    in

    match prog with SProgram(_, A.Parse(A.Block(parse_lines))) ->
        build_parse_body parse_lines;

    the_module
