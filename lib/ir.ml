(** 高级中间表示 (IR) 定义与 AST -> IR 翻译 *)

open Ast

(* ---- IR 操作数 ---- *)
type operand =
  | Imm of int          (* 立即数 *)
  | Temp of int         (* 临时变量/虚拟寄存器 *)
  | Param of string     (* 函数参数 *)
  | Local of string     (* 局部变量 *)

(* ---- IR 指令 ---- *)
type ir_instr =
  | Label of string
  | Ret of operand option
  | Move of operand * operand              (* dest <- src *)
  | BinOp of operand * bin_op * operand * operand
  | UnaryOp of operand * unary_op * operand
  | Store of string * operand              (* 存储到局部变量 *)
  | Load of operand * string               (* 从局部变量加载 *)
  | Alloc of string                        (* 分配局部变量（栈空间） *)

(* ---- 符号表 ---- *)
type var_info = {
  name: string;
  is_const: bool;
  value: int option;   (* 常量的值 *)
}

module StringMap = Map.Make(String)
type symbol_table = var_info StringMap.t

(* 符号表栈（支持作用域嵌套） *)
let symbol_stack : symbol_table list ref = ref []

let enter_scope () =
  symbol_stack := StringMap.empty :: !symbol_stack

let exit_scope () =
  symbol_stack := List.tl !symbol_stack

let add_symbol name is_const value =
  match !symbol_stack with
  | [] -> failwith "No scope to add symbol"
  | current :: rest ->
      let info = { name; is_const; value } in
      symbol_stack := StringMap.add name info current :: rest

let lookup_symbol name =
  let rec search = function
    | [] -> None
    | scope :: rest ->
        match StringMap.find_opt name scope with
        | Some info -> Some info
        | None -> search rest
  in
  search !symbol_stack

(* ---- IR 函数 ---- *)
type ir_func = {
  name      : string;
  ret_type  : func_type;
  params    : string list;
  body      : ir_instr list;
  locals    : string list;   (* 局部变量列表 *)
}

type ir_program = ir_func list

(* ---- 临时变量生成器 ---- *)
let temp_counter = ref 0
let fresh_temp () =
  let t = !temp_counter in
  temp_counter := t + 1;
  Temp t

let reset_temp_counter () = temp_counter := 0

(* ---- 局部变量收集 ---- *)
let local_vars : string list ref = ref []

let add_local_var name =
  if not (List.mem name !local_vars) then
    local_vars := name :: !local_vars

let reset_locals () = local_vars := []

(* ---- AST -> IR 翻译 ---- *)
let rec translate_program (prog : Ast.program) : ir_program =
  let translate_global = function
    | Ast.FuncDef f -> Some (translate_func f)
    | _ -> None
  in
  List.filter_map translate_global prog

and translate_func (f : Ast.func_def) : ir_func =
  reset_temp_counter ();
  reset_locals ();
  symbol_stack := [];  (* 重置符号表 *)
  enter_scope ();      (* 函数级作用域 *)
  
  (* 将参数加入符号表 *)
  List.iter (fun param -> add_symbol param false None) f.f_params;
  
  let body_instrs = translate_stmt f.f_body in
  exit_scope ();
  
  { name      = f.f_name;
    ret_type  = f.f_type;
    params    = f.f_params;
    body      = body_instrs;
    locals    = List.rev !local_vars }

and translate_stmt (s : Ast.stmt) : ir_instr list =
  match s with
  | Ast.Return (Some e) ->
      let (instrs, result) = translate_expr e in
      instrs @ [Ret (Some result)]
  
  | Ast.Return None -> [Ret None]
  
  | Ast.Block stmts ->
      enter_scope ();
      let instrs = List.concat_map translate_stmt stmts in
      exit_scope ();
      instrs
  
  | Ast.EmptyStmt -> []
  
  | Ast.VarDecl (name, init_expr) ->
      (* 检查变量是否已声明 *)
      (match lookup_symbol name with
       | Some _ -> failwith ("Variable " ^ name ^ " already declared in this scope")
       | None -> ());
      
      add_symbol name false None;
      add_local_var name;
      
      let (init_instrs, init_val) = translate_expr init_expr in
      init_instrs @ [Alloc name; Store (name, init_val)]
  
  | Ast.ConstDecl (name, init_expr) ->
      (* 常量折叠：如果初始化表达式是常量，记录其值 *)
      let (init_instrs, init_val) = translate_expr init_expr in
      let const_value = match init_val with
        | Imm n -> Some n
        | _ -> None
      in
      
      (match lookup_symbol name with
       | Some _ -> failwith ("Constant " ^ name ^ " already declared in this scope")
       | None -> ());
      
      add_symbol name true const_value;
      add_local_var name;
      
      init_instrs @ [Alloc name; Store (name, init_val)]
  
  | Ast.Assign (name, expr) ->
      (* 检查变量是否存在且不是常量 *)
      (match lookup_symbol name with
       | None -> failwith ("Undefined variable: " ^ name)
       | Some info when info.is_const -> 
           failwith ("Cannot assign to constant: " ^ name)
       | Some _ -> ());
      
      let (expr_instrs, expr_val) = translate_expr expr in
      expr_instrs @ [Store (name, expr_val)]
  
  | Ast.ExprStmt e ->
      let (instrs, _) = translate_expr e in
      instrs
  
  | _ -> failwith "Step 3: unsupported statement type"

and translate_expr (e : Ast.expr) : ir_instr list * operand =
  match e with
  | Ast.IntLit n -> ([], Imm n)
  
  | Ast.Var name ->
      (* 查找变量 *)
      (match lookup_symbol name with
       | None -> failwith ("Undefined variable: " ^ name)
       | Some info when info.is_const && info.value <> None ->
           (* 常量折叠：直接使用常量值 *)
           ([], Imm (Option.get info.value))
       | Some _ ->
           (* 从内存加载变量 *)
           let dest = fresh_temp () in
           ([Load (dest, name)], dest))
  
  | Ast.Unary (op, e1) ->
      let (instrs1, op1) = translate_expr e1 in
      let dest = fresh_temp () in
      (instrs1 @ [UnaryOp (dest, op, op1)], dest)
  
  | Ast.Binary (op, e1, e2) ->
      let (instrs1, op1) = translate_expr e1 in
      let (instrs2, op2) = translate_expr e2 in
      let dest = fresh_temp () in
      (instrs1 @ instrs2 @ [BinOp (dest, op, op1, op2)], dest)
  
  | _ -> failwith "Step 3: unsupported expression type"