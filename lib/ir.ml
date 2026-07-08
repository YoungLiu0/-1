(*定义中间表示*)
(** 高级中间表示 (IR) 定义与 AST -> IR 翻译 *)

open Ast

(* ---- IR 指令 ---- *)
type ir_instr =
  | Label of string
  | Ret of int option       (* Some n 返回整数常量，None 表示 void 返回 *)
  (* 后续步骤会加入更多指令，如算术、跳转等 *)

(* ---- IR 函数 ---- *)
type ir_func = {
  name      : string;
  ret_type  : func_type;    (* 复用 Ast.func_type *)
  params    : string list;  (* 形式参数名列表 *)
  body      : ir_instr list;
}

type ir_program = ir_func list

(* ---- AST -> IR 翻译 ---- *)
let rec translate_program (prog : Ast.program) : ir_program =
  let translate_global = function
    | Ast.FuncDef f -> Some (translate_func f)
    | _ -> None            (* Step 1 忽略全局变量声明 *)
  in
  List.filter_map translate_global prog
and translate_func (f : Ast.func_def) : ir_func =
  let body_instrs = translate_stmt f.f_body in
  { name      = f.f_name;
    ret_type  = f.f_type;
    params    = f.f_params;
    body      = body_instrs }
and translate_stmt (s : Ast.stmt) : ir_instr list =
  match s with
  | Ast.Return (Some e) ->
      let (instrs, op) = translate_expr e in
      instrs @ [Ret (Some op)]
  | Ast.Return None -> [Ret None]
  | Ast.Block stmts -> List.concat_map translate_stmt stmts
  | Ast.EmptyStmt -> []
  | _ -> failwith "Step 1: only supports return with integer constant"

and translate_expr (e : Ast.expr) : ir_instr list * int =
  match e with
  | Ast.IntLit n -> ([], n)
  | _ -> failwith "Step 1: only supports integer literal in return"