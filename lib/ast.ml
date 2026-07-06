(** 抽象语法树 for ToyC *)

type program = global_def list

and global_def =
  | GlobalVarDecl of string * expr
  | GlobalConstDecl of string * expr
  | FuncDef of func_def

and func_def = {
  f_name: string;
  f_type: func_type;
  f_params: string list;
  f_body: stmt;
}

and func_type = Int | Void

(* 语句 *)
and stmt =
  | Block of stmt list
  | EmptyStmt
  | ExprStmt of expr
  | Assign of string * expr
  | VarDecl of string * expr
  | ConstDecl of string * expr
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option

(* 表达式 *)
and expr =
  | IntLit of int
  | Var of string
  | Unary of unary_op * expr
  | Binary of bin_op * expr * expr
  | Call of string * expr list

and unary_op = Pos | Neg | Not

and bin_op =
  | Add | Sub | Mul | Div | Mod
  | Lt | Le | Gt | Ge | Eq | Ne
  | And | Or

(* ---------- 打印函数（用于 --print-ast） ---------- *)
let rec string_of_expr = function
  | IntLit n -> string_of_int n
  | Var x -> x
  | Unary (Neg, e) -> "-" ^ string_of_expr e
  | Unary (Not, e) -> "!" ^ string_of_expr e
  | Unary (Pos, e) -> "+" ^ string_of_expr e
  | Binary (op, e1, e2) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
        | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">=" | Eq -> "==" | Ne -> "!="
        | And -> "&&" | Or -> "||"
      in
      "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"
  | Call (f, args) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"

let rec string_of_stmt = function
  | Block stmts ->
      "{ " ^ String.concat "; " (List.map string_of_stmt stmts) ^ " }"
  | EmptyStmt -> ";"
  | ExprStmt e -> string_of_expr e ^ ";"
  | Assign (x, e) -> x ^ " = " ^ string_of_expr e ^ ";"
  | VarDecl (x, e) -> "int " ^ x ^ " = " ^ string_of_expr e ^ ";"
  | ConstDecl (x, e) -> "const int " ^ x ^ " = " ^ string_of_expr e ^ ";"
  | If (cond, then_stmt, None) ->
      "if (" ^ string_of_expr cond ^ ") " ^ string_of_stmt then_stmt
  | If (cond, then_stmt, Some else_stmt) ->
      "if (" ^ string_of_expr cond ^ ") " ^ string_of_stmt then_stmt ^ " else " ^ string_of_stmt else_stmt
  | While (cond, body) ->
      "while (" ^ string_of_expr cond ^ ") " ^ string_of_stmt body
  | Break -> "break;"
  | Continue -> "continue;"
  | Return None -> "return;"
  | Return (Some e) -> "return " ^ string_of_expr e ^ ";"

let string_of_program (prog : program) =
  let rec aux = function
    | [] -> ""
    | FuncDef f :: rest ->
        let params = String.concat ", " f.f_params in
        let ret = match f.f_type with Int -> "int" | Void -> "void" in
        "function " ^ ret ^ " " ^ f.f_name ^ "(" ^ params ^ ") " ^ string_of_stmt f.f_body
        ^ "\n" ^ aux rest
    | _ :: rest -> aux rest
  in
  aux prog