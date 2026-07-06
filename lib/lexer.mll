{
open Parser_yacc
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\r' '\n']
let line_comment = "//" [^ '\n']*

rule read = parse

  | whitespace+    { read lexbuf }
  | line_comment   { read lexbuf }

  | "IF"     { IF }
  | "THEN"   { THEN }
  | "ELSE"   { ELSE }
  | "END"    { END }
  | "REPEAT" { REPEAT }
  | "UNTIL"  { UNTIL }
  | "PRINT"  { PRINT }
  | "TRUE"   { TRUE }
  | "FALSE"  { FALSE }

  | ";"   { SEMICOLON }
  | ":="  { ASSIGN }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { TIMES }
  | "/"   { DIVIDE }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "<"   { LT }
  | "="   { EQ }

  | digit+ as num { NUM (int_of_string num) }
  | id as name     { ID name }

  | eof            { EOF }
  | _ as c         { failwith (Printf.sprintf "Lex error: invalid character '%c'" c) }
