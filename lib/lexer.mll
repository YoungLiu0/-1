{
open Parser_yacc
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let number = '0' | ['1'-'9'] digit*
let whitespace = [' ' '\t' '\r' '\n']+

rule read = parse
  | whitespace+        { read lexbuf }          (* 跳过空白 *)
  | "//" [^'\n']*      { read lexbuf }          (* 跳过单行注释 *)
  | "/*"               { comment lexbuf }       (* 多行注释 *)
  
  (* 关键字 *)
  | "const"    { CONST }
  | "int"      { INT }
  | "void"     { VOID }
  | "if"       { IF }
  | "else"     { ELSE }
  | "while"    { WHILE }
  | "break"    { BREAK }
  | "continue" { CONTINUE }
  | "return"   { RETURN }
  
  (* 运算符（注意顺序：长的先匹配） *)
  | "||"       { OR }
  | "&&"       { AND }
  | "<="       { LE }
  | ">="       { GE }
  | "=="       { EQ }
  | "!="       { NE }
  | "<"        { LT }
  | ">"        { GT }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { STAR }
  | "/"        { SLASH }
  | "%"        { PERCENT }
  | "!"        { NOT }
  | "="        { ASSIGN }
  
  (* 分隔符 *)
  | ";"        { SEMI }
  | ","        { COMMA }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "{"        { LBRACE }
  | "}"        { RBRACE }
  
  (* 标识符和数字 *)
  | id         { ID (Lexing.lexeme lexbuf) }
  | number     { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  
  | eof        { EOF }
  | _ as c     { failwith (Printf.sprintf "词法错误: 未知字符 '%c'" c) }

and comment = parse
  | "*/"      { read lexbuf }
  | eof       { failwith "未结束的多行注释" }
  | _         { comment lexbuf }