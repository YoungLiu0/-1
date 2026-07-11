(** 高级中间表示 (IR) 定义与 AST -> IR 翻译 *)

open Ast

(* ---- IR 操作数 ---- *)
type operand =
  | Imm of int          (* 立即数 *)
  | Temp of int         (* 临时变量/虚拟寄存器 *)
  | Param of string     (* 函数参数 *)
  | Local of string     (* 局部变量 *)
  | Global of string    (* 全局变量 *)

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
  (* 控制流指令 *)
  | Jump of string                         (* 无条件跳转 *)
  | BranchZero of operand * string         (* if operand == 0 goto label *)
  | BranchNonZero of operand * string      (* if operand != 0 goto label *)
  (* 函数调用与全局变量 *)
  | Call of operand * string * operand list  (* dest = func_name(args) *)
  | LoadGlobal of operand * string          (* 从全局变量加载 *)
  | StoreGlobal of string * operand         (* 存储到全局变量 *)

(* ---- 符号表 ---- *)
type var_info = {
  name: string;
  is_const: bool;
  value: int option;   (* 常量的值 *)
  is_global: bool;     (* 是否为全局变量 *)
  ir_name:string;
}

module StringMap = Map.Make(String)
type symbol_table = var_info StringMap.t

(* 符号表栈（支持作用域嵌套） *)
let symbol_stack : symbol_table list ref = ref []

(* 全局变量表 *)

type ir_global = {
  g_name    : string;
  g_is_const: bool;
  g_init    : int option;    (* 初始值 *)
}
let unique_var_counter = ref 0


let global_vars : ir_global list ref = ref []
let add_global_var name is_const value =
  global_vars := { g_name = name; g_is_const = is_const; g_init = value } :: !global_vars
let enter_scope () =
  symbol_stack := StringMap.empty :: !symbol_stack
  let exit_scope () =
  symbol_stack := List.tl !symbol_stack


  let lookup_symbol name =
  let rec search = function
    | [] -> None
    | scope :: rest ->
        match StringMap.find_opt name scope with
        | Some info -> Some info
        | None -> search rest
  in
  search !symbol_stack

let lookup_current_scope name=
  match !symbol_stack with
  |[]->None
  |current::_->StringMap.find_opt name current

let add_symbol name is_const value is_global =
  match !symbol_stack with
  | [] -> failwith "No scope to add symbol"
  | current :: rest ->
    let ir_name = 
      if lookup_symbol name <> None then
        let new_name = name ^"_"^string_of_int !unique_var_counter in
    incr unique_var_counter;
    new_name
      else name in
      let info = { name; is_const; value; is_global ;ir_name} in
      symbol_stack := StringMap.add name info current :: rest



let get_ir_name name=
match lookup_symbol name with
|Some info -> info.ir_name
|None -> failwith("Undefined Variable"^name)
let reset_globals () = global_vars := []

(* ---- IR 函数 ---- *)
type ir_func = {
  name      : string;
  ret_type  : func_type;
  params    : string list;
  body      : ir_instr list;
  locals    : string list;   (* 局部变量列表 *)
}



type ir_program = {
  globals   : ir_global list;
  functions : ir_func list;
}

(* ---- 临时变量和标签生成器 ---- *)
let temp_counter = ref 0
let label_counter = ref 0

let fresh_temp () =
  let t = !temp_counter in
  temp_counter := t + 1;
  Temp t

let fresh_label prefix =
  let n = !label_counter in
  label_counter := n + 1;
  Printf.sprintf "%s_%d" prefix n

let reset_temp_counter () = temp_counter := 0
let reset_label_counter () = label_counter := 0

(* ---- 局部变量收集 ---- *)
let local_vars : string list ref = ref []

let add_local_var name =
  if not (List.mem name !local_vars) then
    local_vars := name :: !local_vars

let reset_locals () = local_vars := []

(* ---- 循环上下文（用于 break/continue）---- *)
type loop_context = {
  continue_label : string;
  break_label    : string;
}

let loop_stack : loop_context list ref = ref []

let enter_loop continue_lbl break_lbl =
  loop_stack := { continue_label = continue_lbl; break_label = break_lbl } :: !loop_stack

let exit_loop () =
  loop_stack := List.tl !loop_stack

let current_loop () =
  match !loop_stack with
  | [] -> None
  | ctx :: _ -> Some ctx

(* ---- AST -> IR 翻译 ---- *)
let rec translate_program (prog : Ast.program) : ir_program =
  reset_globals ();
  
  (* 第一遍：收集所有全局变量 *)
  let collect_global = function
    | Ast.GlobalVarDecl (name, init_expr) ->
        let init_val = match init_expr with
          | Ast.IntLit n -> Some n
          | _ -> None
        in
        add_global_var name false init_val
    | Ast.GlobalConstDecl (name, init_expr) ->
        let init_val = match init_expr with
          | Ast.IntLit n -> Some n
          | _ -> None
        in
        add_global_var name true init_val
    | _ -> ()
  in
  List.iter collect_global prog;
  
  (* 第二遍：翻译函数 *)
  let functions = List.filter_map (function
    | Ast.FuncDef f -> Some (translate_func f)
    | _ -> None
  ) prog in
  
  let globals = List.map (fun g ->
    { g_name = g.g_name; g_is_const = g.g_is_const; g_init = g.g_init }
  ) (List.rev !global_vars) in
  
  { globals; functions }

and translate_func (f : Ast.func_def) : ir_func =
  reset_temp_counter ();
  reset_label_counter ();
  reset_locals ();
  symbol_stack := [];
  loop_stack := [];
  enter_scope ();      (* 函数级作用域 *)
  (* 将全局变量加入符号表（所有函数都能看到全局变量） *)
  List.iter (fun g ->
    add_symbol g.g_name g.g_is_const g.g_init true
  ) (List.rev !global_vars);
  (* 将参数加入符号表，参数视为局部变量，需要栈空间 *)
  List.iter (fun param -> 
    add_symbol param false None false;
    add_local_var param
  ) f.f_params;
  
  let body_instrs = translate_stmt f.f_body in
  
  (* 为 void 函数自动添加 return，避免控制流末端悬空 *)
  let body_instrs =
    if f.f_type = Ast.Void then
      match List.rev body_instrs with
      | Ret _ :: _ -> body_instrs   (* 已显式 return *)
      | _ -> body_instrs @ [Ret None]
    else body_instrs
  in
  
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
      (match lookup_current_scope name with
       | Some _ -> failwith ("Variable " ^ name ^ " already declared")
       | None -> ());
       let (init_instrs, init_val) = translate_expr init_expr in
      add_symbol name false None false;
      let ir_name = get_ir_name name in
      add_local_var ir_name;
      init_instrs @ [Alloc ir_name; Store (ir_name, init_val)]
  
  | Ast.ConstDecl (name, init_expr) ->
      let (init_instrs, init_val) = translate_expr init_expr in
      let const_value = match init_val with Imm n -> Some n | _ -> None in
      (match lookup_current_scope name with
       | Some _ -> failwith ("Constant " ^ name ^ " already declared")
       | None -> ());
      add_symbol name true const_value false;
      let ir_name = get_ir_name name in
      add_local_var ir_name;
      init_instrs @ [Alloc ir_name; Store (ir_name, init_val)]
  
  | Ast.Assign (name, expr) ->
      (match lookup_symbol name with
       | None -> failwith ("Undefined variable: " ^ name)
       | Some info when info.is_const -> 
           failwith ("Cannot assign to constant: " ^ name)
       | Some info when info.is_global ->
           (* 全局变量赋值 *)
           let (expr_instrs, expr_val) = translate_expr expr in
           expr_instrs @ [StoreGlobal (name, expr_val)]
       | Some info->
           (* 局部变量赋值 *)
           let ir_name = info.ir_name in
           let (expr_instrs, expr_val) = translate_expr expr in
           expr_instrs @ [Store (ir_name, expr_val)])
  
  | Ast.ExprStmt e ->
      let (instrs, _) = translate_expr e in
      instrs
  
  (* if 语句 *)
  | Ast.If (cond, then_stmt, else_opt) ->
      let then_label = fresh_label "then" in
      let else_label = fresh_label "else" in
      let end_label = fresh_label "if_end" in
      
      let (cond_instrs, cond_result) = translate_expr cond in
      let then_instrs = translate_stmt then_stmt in
      
      (match else_opt with
       | None ->
           cond_instrs @
           [BranchZero (cond_result, end_label);
            Label then_label] @
           then_instrs @
           [Label end_label]
       
       | Some else_stmt ->
           let else_instrs = translate_stmt else_stmt in
           cond_instrs @
           [BranchZero (cond_result, else_label);
            Label then_label] @
           then_instrs @
           [Jump end_label;
            Label else_label] @
           else_instrs @
           [Label end_label])
  
  (* while 循环 *)
  | Ast.While (cond, body) ->
      let loop_start = fresh_label "while_start" in
      let loop_body = fresh_label "while_body" in
      let loop_end = fresh_label "while_end" in
      
      enter_loop loop_start loop_end;
      
      let (cond_instrs, cond_result) = translate_expr cond in
      let body_instrs = translate_stmt body in
      
      exit_loop ();
      
      [Label loop_start] @
      cond_instrs @
      [BranchZero (cond_result, loop_end);
       Label loop_body] @
      body_instrs @
      [Jump loop_start;
       Label loop_end]
  
  | Ast.Break ->
      (match current_loop () with
       | None -> failwith "break outside of loop"
       | Some ctx -> [Jump ctx.break_label])
  
  | Ast.Continue ->
      (match current_loop () with
       | None -> failwith "continue outside of loop"
       | Some ctx -> [Jump ctx.continue_label])


(* 短路求值 + 函数调用 + 全局变量访问 *)
and translate_expr (e : Ast.expr) : ir_instr list * operand =
  match e with
  | Ast.IntLit n -> ([], Imm n)
  
  | Ast.Var name ->
      (match lookup_symbol name with
       | None -> failwith ("Undefined variable: " ^ name)
       | Some info when info.is_const && info.value <> None ->
           ([], Imm (Option.get info.value))
       | Some info when info.is_global ->
           let dest = fresh_temp () in
           ([LoadGlobal (dest, name)], dest)
       | Some info->
        let ir_name = info.ir_name in
           let dest = fresh_temp () in
           ([Load (dest, ir_name)], dest))
  
  | Ast.Unary (op, e1) ->
      let (instrs1, op1) = translate_expr e1 in
      let dest = fresh_temp () in
      (instrs1 @ [UnaryOp (dest, op, op1)], dest)
  
  (* && 短路求值 *)
  | Ast.Binary (Ast.And, e1, e2) ->
      let result = fresh_temp () in
      let false_label = fresh_label "and_false" in
      let end_label = fresh_label "and_end" in
      
      let (instrs1, op1) = translate_expr e1 in
      let (instrs2, op2) = translate_expr e2 in
      
      instrs1 @
      [BranchZero (op1, false_label)] @
      instrs2 @
      [Move (result, op2);
       Jump end_label;
       Label false_label;
       Move (result, Imm 0);
       Label end_label],
      result
  
  (* || 短路求值 *)
  | Ast.Binary (Ast.Or, e1, e2) ->
      let result = fresh_temp () in
      let true_label = fresh_label "or_true" in
      let end_label = fresh_label "or_end" in
      
      let (instrs1, op1) = translate_expr e1 in
      let (instrs2, op2) = translate_expr e2 in
      
      instrs1 @
      [BranchNonZero (op1, true_label)] @
      instrs2 @
      [Move (result, op2);
       Jump end_label;
       Label true_label;
       Move (result, Imm 1);
       Label end_label],
      result
  
  | Ast.Binary (op, e1, e2) ->
      let (instrs1, op1) = translate_expr e1 in
      let (instrs2, op2) = translate_expr e2 in
      let dest = fresh_temp () in
      (instrs1 @ instrs2 @ [BinOp (dest, op, op1, op2)], dest)
  
  | Ast.Call (func_name, args) ->
      let arg_instrs = List.map translate_expr args in
      let instrs = List.concat_map fst arg_instrs in
      let arg_ops = List.map snd arg_instrs in
      let dest = fresh_temp () in
      (instrs @ [Call (dest, func_name, arg_ops)], dest)

