open Toylanglib

let usage = "Usage: toylang [options] <file>"
let file = ref ""
let parser_type = ref "yacc"
let print_ast = ref false
let check_types = ref false

let options =
  [ ( "--parser"
    , Arg.Symbol ([ "yacc"; "descent" ], fun s -> parser_type := s)
    , " Choose parser type: yacc or descent (default: yacc)" )
  ; ( "--print-ast"
    , Arg.Set print_ast
    , " Print the Abstract Syntax Tree (AST) instead of interpreting" )
  ; "--check-types", Arg.Set check_types, " Perform type checking instead of interpreting"
  ]
;;

let parse_args () =
  let anon_fun filename =
    if !file = ""
    then file := filename
    else raise (Arg.Bad "Only one input file can be specified")
  in
  Arg.parse options anon_fun usage;
  if !file = "" then raise (Arg.Bad "No input file specified")
;;

let () =
  try
    parse_args ();
    let lexbuf = Lexing.from_channel (open_in !file) in
    let ast = Parser_yacc.program Lexer.read lexbuf in
    if !print_ast then
      Printf.printf "%s\n" (Ast.string_of_program ast)
    else if !check_types then
      Printf.eprintf "Type checking not implemented yet.\n"
    else
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