open Ir
open Cfg
open Loop_unrolling
open Loop_var_cache
(* ========== 辅助函数 ========== *)
(* 判断指令是否有副作用（不可删除） *)
let has_side_effect = function
  | Ret _ | Call _ | StoreGlobal _ | Jump _ 
  | BranchZero _ | BranchNonZero _ | Label _ -> true
  | Store _ -> true  (* 保守处理：认为所有 Store 都有副作用 *)
  | _ -> false
(** 从操作数获取立即数值 *)
let get_imm = function
  | Imm n -> Some n
  | _ -> None
(* ========== 局部优化========== *)
(*局部公共子表达式消除*)
let local_cse (instrs : ir_instr list) : ir_instr list =
  let cache = Hashtbl.create 16 in
  
  let clear_cache_for_def = function
    | Temp t -> 
        let var = "t" ^ string_of_int t in
        (* 删除所有涉及该变量的缓存 *)
        Hashtbl.filter_map_inplace (fun _expr cached_dest ->
          match cached_dest with
          | Temp t' when "t" ^ string_of_int t' = var -> None
          | Local v when "l_" ^ v = var -> None
          | _ -> Some cached_dest
        ) cache
    | Local v ->
        let var = "l_" ^ v in
        Hashtbl.filter_map_inplace (fun _expr cached_dest ->
          match cached_dest with
          | Temp t when "t" ^ string_of_int t = var -> None
          | Local v' when "l_" ^ v' = var -> None
          | _ -> Some cached_dest
        ) cache
    | _ -> ()
  in
  
  List.map (function
    | BinOp (dest, op, op1, op2) as instr ->
        let expr = (op, op1, op2) in
        (match Hashtbl.find_opt cache expr with
         | Some cached_dest when cached_dest <> dest ->
             (* 找到重复表达式，替换为 Move *)
             clear_cache_for_def dest;
             Move (dest, cached_dest)
         | _ ->
             (* 记录新表达式 *)
             clear_cache_for_def dest;
             Hashtbl.replace cache expr dest;
             instr)
    
    (* 遇到定义，清理相关缓存 *)
    | Move (dest, _) as instr ->
        clear_cache_for_def dest;
        instr
    
    | UnaryOp (dest, _, _) as instr ->
        clear_cache_for_def dest;
        instr
    
    | Load (dest, _) as instr ->
        clear_cache_for_def dest;
        instr
    
    | Call (dest, _, _) as instr ->
        clear_cache_for_def dest;
        Hashtbl.clear cache;  (* 函数调用可能改变任何值 *)
        instr
    
    | Store _ | StoreGlobal _ as instr ->
        Hashtbl.clear cache;  (* 保守处理：清空所有缓存 *)
        instr
    
    | instr -> instr
  ) instrs
(*常量折叠 - 编译时计算常量表达式 *)
let constant_folding (instrs : ir_instr list) : ir_instr list =
  List.map (function
    | BinOp (dest, op, Imm n1, Imm n2) ->
        let result = match op with
          | Ast.Add -> n1 + n2
          | Ast.Sub -> n1 - n2
          | Ast.Mul -> n1 * n2
          | Ast.Div -> if n2 = 0 then 0 else n1 / n2
          | Ast.Mod -> if n2 = 0 then 0 else n1 mod n2
          | Ast.Lt -> if n1 < n2 then 1 else 0
          | Ast.Le -> if n1 <= n2 then 1 else 0
          | Ast.Gt -> if n1 > n2 then 1 else 0
          | Ast.Ge -> if n1 >= n2 then 1 else 0
          | Ast.Eq -> if n1 = n2 then 1 else 0
          | Ast.Ne -> if n1 <> n2 then 1 else 0
          | _ -> failwith "Unsupported operator in constant folding"
        in
        Move (dest, Imm result)
    
    | UnaryOp (dest, op, Imm n) ->
        let result = match op with
          | Ast.Neg -> -n
          | Ast.Not -> if n = 0 then 1 else 0
          | Ast.Pos -> n
        in
        Move (dest, Imm result)
    
    | instr -> instr
  ) instrs
(*局部常量传播：将已知的常量临时变量替换为立即数 *)
let local_constant_propagation (instrs : ir_instr list) : ir_instr list =
  let const_map = Hashtbl.create 16 in
  List.map (fun instr ->
    match instr with
    (* 如果遇到 Move(dest, Imm n)，记录这个 dest 是常量 n *)
    | Move (dest, Imm n) ->
        Hashtbl.replace const_map dest n;
        instr
    (* 对于任何指令，尝试将其操作数中的已知常量替换为 Imm *)
    | _ ->
        let replace_op op =
          match op with
          | Temp t -> (try Imm (Hashtbl.find const_map (Temp t)) with Not_found -> op)
          | _ -> op
        in
        let instr' = match instr with
          | BinOp (dest, op, op1, op2) ->
              let op1' = replace_op op1 in
              let op2' = replace_op op2 in
              (* 如果替换后两个操作数都是 Imm，可以直接计算并变为 Move *)
              (match op1', op2' with
               | Imm n1, Imm n2 ->
                   let result = begin match op with
                     | Ast.Add -> n1 + n2 | Ast.Sub -> n1 - n2
                     | Ast.Mul -> n1 * n2 | Ast.Div -> if n2=0 then 0 else n1/n2
                     | Ast.Mod -> if n2=0 then 0 else n1 mod n2
                     | Ast.Lt -> if n1<n2 then 1 else 0
                     | Ast.Le -> if n1<=n2 then 1 else 0
                     | Ast.Gt -> if n1>n2 then 1 else 0
                     | Ast.Ge -> if n1>=n2 then 1 else 0
                     | Ast.Eq -> if n1=n2 then 1 else 0
                     | Ast.Ne -> if n1<>n2 then 1 else 0
                     | _ -> failwith "unsupported"
                   end in
                   Move (dest, Imm result)
               | _ -> BinOp (dest, op, op1', op2'))
          | UnaryOp (dest, op, op1) ->
              let op1' = replace_op op1 in
              (match op1' with
               | Imm n ->
                   let result = match op with
                     | Ast.Neg -> -n | Ast.Not -> if n=0 then 1 else 0
                     | Ast.Pos -> n | _ -> failwith "unsupported"
                   in
                   Move (dest, Imm result)
               | _ -> UnaryOp (dest, op, op1'))
          | _ -> instr
        in
        (* 如果指令定义了一个新变量，从 const_map 中移除（因为值可能改变） *)
        (match instr' with
         | Move (dest, _) | BinOp (dest, _, _, _) | UnaryOp (dest, _, _)
         | Load (dest, _) | LoadGlobal (dest, _) | Call (dest, _, _) ->
             Hashtbl.remove const_map dest
         | _ -> ());
        instr'
  ) instrs
(*代数化简 - 基本恒等式 *)
let algebraic_simplification (instrs : ir_instr list) : ir_instr list =
  List.map (function
    (* x + 0 = x *)
    | BinOp (dest, Ast.Add, op, Imm 0) -> Move (dest, op)
    | BinOp (dest, Ast.Add, Imm 0, op) -> Move (dest, op)
    
    (* x - 0 = x *)
    | BinOp (dest, Ast.Sub, op, Imm 0) -> Move (dest, op)
    
    (* x * 0 = 0 *)
    | BinOp (dest, Ast.Mul, _, Imm 0) -> Move (dest, Imm 0)
    | BinOp (dest, Ast.Mul, Imm 0, _) -> Move (dest, Imm 0)
    
    (* x * 1 = x *)
    | BinOp (dest, Ast.Mul, op, Imm 1) -> Move (dest, op)
    | BinOp (dest, Ast.Mul, Imm 1, op) -> Move (dest, op)
    
    (* x / 1 = x *)
    | BinOp (dest, Ast.Div, op, Imm 1) -> Move (dest, op)
    
    (* 其他保持不变 *)
    | instr -> instr
  ) instrs
(* 局部无用 Move 消除：Move(x, x) 是无操作 *)
let eliminate_trivial_moves (instrs : ir_instr list) : ir_instr list =
  List.filter (function
    | Move (Temp t1, Temp t2) when t1 = t2 -> false
    | Move (Local v1, Local v2) when v1 = v2 -> false
    | _ -> true
  ) instrs
(* 局部存储转发 + 前向值替换 *)
let store_load_forwarding (instrs : ir_instr list) : ir_instr list =
  (* 维护变量名 -> 当前已知值的映射 *)
  let value_map = Hashtbl.create 16 in
  (* 如果某指令可能修改内存，则清空映射 *)
  let invalidate_all () = Hashtbl.clear value_map in
  List.map (fun instr ->
    match instr with
     | Label _ ->instr;
    (* 遇到 Store，记录这个变量的值，并生成指令 *)
    | Store (var, src) ->
        Hashtbl.replace value_map var src;
        instr   (* 暂时保留，后续死代码删除会移除无用 Store *)
    (* 遇到 Load，如果映射中有值，则替换为 Move *)
    | Load (dest, var) ->
        (match Hashtbl.find_opt value_map var with
         | Some src ->
             (* 用 Move 代替 Load，并从映射中移除（因为值被读取后，后续 Load 可以继续使用，除非有 Store 改变？保守起见不移除，但遇到 Store 会更新映射，所以安全） *)
             Move (dest, src)
         | None -> instr)
    (* 全局 Store 和函数调用会破坏局部变量假设，清空映射 *)
    | StoreGlobal _ | Call _ ->
        invalidate_all ();
        instr
    (* 所有其他指令保持不变，但注意：如果指令定义了某个映射中的变量（如 Move 或 BinOp 的目标是 Local），是否需要更新？这里我们只处理 Store/Load 变量，其他变量不影响映射 *)
    | _ -> instr
  ) instrs
(* 组合所有局部优化 *)
let optimize_local_block (instrs : ir_instr list) : ir_instr list =
  instrs
  |> constant_folding
  |> local_constant_propagation   (* 新增：传播常量 *)
  |> algebraic_simplification
  |> store_load_forwarding   (* 新增存储转发 *)
  |> local_cse              (* 添加局部CSE *)
  |> eliminate_trivial_moves
(* ========== 全局优化 ========== *)
(* 不可达代码消除-基于 CFG 的可达性分析 *)
let unreachable_code_elimination (cfg : Cfg.t) : Cfg.t =
  let reachable = Hashtbl.create (List.length cfg.labels) in
  
  let rec dfs lbl =
    if not (Hashtbl.mem reachable lbl) then begin
      Hashtbl.add reachable lbl true;
      let succs = Cfg.get_successors cfg lbl in
      List.iter dfs succs
    end
  in
  
  dfs cfg.entry;
  
  let labels' = List.filter (fun lbl -> Hashtbl.mem reachable lbl) cfg.labels in
  let blocks' = Hashtbl.create (List.length labels') in
  let preds' = Hashtbl.create (List.length labels') in
  let succs' = Hashtbl.create (List.length labels') in
  
  List.iter (fun lbl ->
    Hashtbl.add blocks' lbl (Cfg.get_block cfg lbl);
    let pred_list = Cfg.get_predecessors cfg lbl in
    let succ_list = Cfg.get_successors cfg lbl in
    Hashtbl.add preds' lbl (List.filter (fun p -> Hashtbl.mem reachable p) pred_list);
    Hashtbl.add succs' lbl (List.filter (fun s -> Hashtbl.mem reachable s) succ_list)
  ) labels';
  
  { blocks = blocks'; labels = labels'; entry = cfg.entry; preds = preds'; succs = succs' }
(*死代码消除 - 基于活跃变量分析 *)
let dead_code_elimination (cfg : Cfg.t) : Cfg.t =
  (* 执行活跃变量分析 *)
  let live_result = Live_vars.analyze cfg in
  let blocks' = Hashtbl.create (Hashtbl.length cfg.blocks) in
  List.iter (fun lbl ->
    let block = Cfg.get_block cfg lbl in
    let live_out = try Hashtbl.find live_result.out_facts lbl with Not_found -> [] in
    (* 反向扫描，计算每条指令后的活跃变量 *)
    let rec process_instrs instrs live_after acc =
      match instrs with
      | [] -> acc
      | instr :: rest ->
          (* 提取定义的变量 *)
          let def_var = match instr with
            | Store (var, _) -> Some ("l_" ^ var)
            | StoreGlobal (var, _) -> Some ("g_" ^ var)
            | Alloc var -> Some ("l_" ^ var)
            | Move (Local var, _) -> Some ("l_" ^ var)
            | Move (Temp t, _) -> Some ("t" ^ string_of_int t)
            | BinOp (Local var, _, _, _) -> Some ("l_" ^ var)
            | BinOp (Temp t, _, _, _) -> Some ("t" ^ string_of_int t)
            | UnaryOp (Local var, _, _) -> Some ("l_" ^ var)
            | UnaryOp (Temp t, _, _) -> Some ("t" ^ string_of_int t)
            | Call (Local var, _, _) -> Some ("l_" ^ var)
            | Call (Temp t, _, _) -> Some ("t" ^ string_of_int t)
            | Load (Local var, _) -> Some ("l_" ^ var)
            | Load (Temp t, _) -> Some ("t" ^ string_of_int t)
            | LoadGlobal (Local var, _) -> Some ("l_" ^ var)
            | LoadGlobal (Temp t, _) -> Some ("t" ^ string_of_int t)
            | _ -> None
          in
          (* 判断是否为死代码 *)
          let is_temp_def = function
  | Some v -> String.length v > 0 && v.[0] = 't'
  | None -> false
in
let is_dead = match def_var with
  | Some v -> not (List.mem v live_after) && not (has_side_effect instr) && not (is_temp_def (Some v))
  | None -> false
          in
          if is_dead then
            (* 跳过死指令 *)
            process_instrs rest live_after acc
          else begin
            (* 计算该指令前的活跃变量 *)
            let uses = Live_vars.get_uses instr in
            let live_before = 
              Live_vars.LiveLattice.meet uses 
                (match def_var with
                 | Some v -> List.filter (fun x -> x <> v) live_after
                 | None -> live_after)
            in
            process_instrs rest live_before (instr :: acc)
          end
    in
    let filtered_instrs = process_instrs (List.rev block.instrs) live_out [] in
    Hashtbl.add blocks' lbl { block with instrs = filtered_instrs }
  ) cfg.labels;
  { cfg with blocks = blocks' }

(*其他优化*)
(* 尾递归消除：将递归调用转为跳转，复用栈帧 *)
let tail_call_elimination (func : Ir.ir_func) : Ir.ir_func =
  let entry_label = func.name ^ "_entry" in
  let rec has_tail_call = function
    | (Ir.Call (_, fname, _)) :: (Ir.Ret (Some _)) :: _ when fname = func.name -> true
    | _ :: rest -> has_tail_call rest
    | [] -> false
  in
  if not (has_tail_call func.body) then func
  else
    let rec replace_tail_calls acc = function
      | (Ir.Call (dest, fname, args)) :: (Ir.Ret (Some dest')) :: rest
        when fname = func.name && dest = dest' ->
          let stores =
            try List.map2 (fun param arg -> Ir.Store (param, arg)) func.params args
            with Invalid_argument _ -> []
          in
          if stores = [] then
            replace_tail_calls (Ir.Ret (Some dest') :: Ir.Call (dest, fname, args) :: acc) rest
          else
            let new_instrs = stores @ [Ir.Jump entry_label] in
            replace_tail_calls (List.rev_append new_instrs acc) rest
      | instr :: rest -> replace_tail_calls (instr :: acc) rest
      | [] -> List.rev acc
    in
    let new_body = Ir.Label entry_label :: replace_tail_calls [] func.body in
    { func with body = new_body }
  (* ========== 主优化流程 ========== *)
(* 对 CFG 执行局部优化 *)
let optimize_cfg_local (cfg : Cfg.t) : Cfg.t =
  let blocks' = Hashtbl.create (Hashtbl.length cfg.blocks) in
  
  List.iter (fun lbl ->
    let block = Cfg.get_block cfg lbl in
    let optimized_instrs = optimize_local_block block.instrs in
    Hashtbl.add blocks' lbl { block with instrs = optimized_instrs }
  ) cfg.labels;
  
  { cfg with blocks = blocks' }
(* 完整优化流程：局部优化 → 全局优化（迭代一次） *)
let optimize_cfg (cfg : Cfg.t) : Cfg.t =
  cfg
  |> optimize_cfg_local
  |> Loop_unrolling.unroll_loops_once    (* 循环展开 *)
  |> Loop_var_cache.cache_loop_vars_in_cfg    (* 循环变量寄存器缓存 *)
  (* |> Const_prop.global_constant_propagation  (* 全局常量传播 *)
  |> Copy_prop.global_copy_propagation       全局拷贝传播 *)
  |> optimize_cfg_local                (* 传播后再做一遍局部优化 *)
  |> unreachable_code_elimination
  |> dead_code_elimination
  |> optimize_cfg_local  
(* 简要描述指令，用于调试 *)
let string_of_instr = function
  | Ir.Label s -> "Label " ^ s
  | Ir.Jump s -> "Jump " ^ s
  | Ir.BranchZero (_, s) -> "BranchZero -> " ^ s
  | Ir.BranchNonZero (_, s) -> "BranchNonZero -> " ^ s
  | Ir.Ret (Some _) -> "Ret(Some)"
  | Ir.Ret None -> "Ret(None)"
  | Ir.Move (_, _) -> "Move"
  | Ir.BinOp (_, op, _, _) -> "BinOp " ^ (match op with Ast.Add -> "+" | Ast.Sub -> "-" | _ -> "?")
  | Ir.UnaryOp (_, _op, _) -> "UnaryOp"
  | Ir.Store (_, _) -> "Store"
  | Ir.Load (_, _) -> "Load"
  | Ir.Alloc _ -> "Alloc"
  | Ir.Call (_, _, _) -> "Call"
  | _ -> "Other"
let dump_instrs title instrs =
  Printf.eprintf "=== %s ===\n" title;
  List.iter (fun i -> Printf.eprintf "%s\n" (string_of_instr i)) instrs;
  Printf.eprintf "=== end %s ===\n" title
(*优化器的主入口*)
let optimize_func (func : Ir.ir_func) : Ir.ir_func =
   let func = tail_call_elimination func in
  Printf.eprintf "[OPT] optimizing %s (%d instrs)\n" func.name (List.length func.body);
  let cfg = Cfg_builder.build_cfg func in
  let optimized_cfg = optimize_cfg cfg in
  let body = Cfg.cfg_to_linear optimized_cfg in
  { func with body }
