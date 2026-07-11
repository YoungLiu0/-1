open Toylanglib

let usage = "Usage: toylang [options] <file>"
let file = ref ""
let print_ast = ref false
let check_types = ref false
let emit_asm = ref false
let enable_opt = ref false

let options =
  [ ( "--print-ast"
    , Arg.Set print_ast
    , " Print the Abstract Syntax Tree (AST) instead of interpreting" )
  ; "--check-types", Arg.Set check_types, " Perform type checking instead of interpreting"
  ; "--emit-asm", Arg.Set emit_asm, " Compile to RISC-V assembly and output"
  ; "-opt", Arg.Set enable_opt, " Enable optimizations"
  ]
;;

let parse_args () =
  let anon_fun filename =
    if !file = ""
    then file := filename
    else raise (Arg.Bad "Only one input file can be specified")
  in
  Arg.parse options anon_fun usage
;;

let () =
  try
    parse_args ();
    let lexbuf =
      if !file = "" then Lexing.from_channel stdin
      else Lexing.from_channel (open_in !file)
    in
    let ast = Parser_yacc.program Lexer.read lexbuf in
    if !print_ast then
      Printf.printf "%s\n" (Ast.string_of_program ast)
    else if !check_types then
      Printf.eprintf "Type checking not implemented yet.\n"
    else if !emit_asm then begin
      let ir_prog = Ir.translate_program ast in
      let optimized_funcs =
        if !enable_opt then
          List.map Ir_optimizer.optimize_func ir_prog.functions
        else
          ir_prog.functions
      in
      let mach_prog = Select.select_program { ir_prog with functions = optimized_funcs } in
      let alloc_funcs = List.map Regalloc.allocate_registers mach_prog.functions in
     let asm = Emit_riscv.emit_program mach_prog.globals alloc_funcs in
      print_string asm;
      flush stdout
    end else
      Printf.eprintf "Interpretation not implemented yet.\n"
  with
  | Arg.Bad msg ->
    Printf.eprintf "%s: %s\n" Sys.argv.(0) msg;
    Arg.usage options usage;
    exit 2
  | Parser_yacc.Error ->
      Printf.eprintf "Parse error\n";
      exit 1
  | Failure msg | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      exit 1