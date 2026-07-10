(* 中端优化,在 CFG 上跑常量传播、死代码删除*)
(** IR 优化器（Step 1 直接返回原 CFG，不做任何优化） *)


(** IR 优化器 - 保守但正确的实现 *)

open Ir
open Cfg

(** ========== 辅助函数 ========== *)

(** 判断指令是否有副作用（不可删除） *)
let has_side_effect = function
  | Ret _ | Call _ | StoreGlobal _ | Jump _ 
  | BranchZero _ | BranchNonZero _ | Label _ -> true
  | Store _ -> true  (* 保守处理：认为所有 Store 都有副作用 *)
  | _ -> false

(** 从操作数获取立即数值 *)
let get_imm = function
  | Imm n -> Some n
  | _ -> None

(** ========== 局部优化（安全的部分）========== *)

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

(** 常量折叠 - 编译时计算常量表达式 *)
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

(** 代数化简 - 基本恒等式 *)
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

(** 局部无用 Move 消除：Move(x, x) 是无操作 *)
let eliminate_trivial_moves (instrs : ir_instr list) : ir_instr list =
  List.filter (function
    | Move (Temp t1, Temp t2) when t1 = t2 -> false
    | Move (Local v1, Local v2) when v1 = v2 -> false
    | _ -> true
  ) instrs

(** 组合所有局部优化 *)
let optimize_local_block (instrs : ir_instr list) : ir_instr list =
  instrs
  |> constant_folding
  |> algebraic_simplification
  |> local_cse              (* 添加局部CSE *)
  |> eliminate_trivial_moves

(** ========== 全局优化 ========== *)

(** 不可达代码消除 - 基于 CFG 的可达性分析 *)
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

(** 简单的死代码消除 - 基于活跃变量分析 *)
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
          let is_dead = match def_var with
            | Some v -> not (List.mem v live_after) && not (has_side_effect instr)
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

(** 局部公共子表达式消除（在单个基本块内） *)

(** ========== 主优化流程 ========== *)

(** 对 CFG 执行局部优化 *)
let optimize_cfg_local (cfg : Cfg.t) : Cfg.t =
  let blocks' = Hashtbl.create (Hashtbl.length cfg.blocks) in
  
  List.iter (fun lbl ->
    let block = Cfg.get_block cfg lbl in
    let optimized_instrs = optimize_local_block block.instrs in
    Hashtbl.add blocks' lbl { block with instrs = optimized_instrs }
  ) cfg.labels;
  
  { cfg with blocks = blocks' }

(** 完整优化流程：局部优化 → 全局优化（迭代一次） *)
let optimize_cfg (cfg : Cfg.t) : Cfg.t =
  cfg
  |> optimize_cfg_local
  |> Const_prop.global_constant_propagation  (* 全局常量传播 *)
  |> Copy_prop.global_copy_propagation       (* 全局拷贝传播 *)
  |> optimize_cfg_local                (* 传播后再做一遍局部优化 *)
  |> unreachable_code_elimination
  |> dead_code_elimination
  |> optimize_cfg_local  (* 再做一遍局部优化清理 *)

  (** 对单个 ir_func 进行优化，返回优化后的 ir_func *)

let string_of_instr = function
  | Label s -> "Label " ^ s
  | Jump s -> "Jump " ^ s
  | BranchZero (_, s) -> "BranchZero -> " ^ s
  | BranchNonZero (_, s) -> "BranchNonZero -> " ^ s
  | Ret _ -> "Ret"
  | Move (_, _) -> "Move"
  | BinOp (_, op, _, _) -> "BinOp " ^ (match op with Ast.Add -> "+" | _ -> "?")
  | _ -> "OtherInstr";;
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
  | Ir.UnaryOp (_, op, _) -> "UnaryOp"
  | Ir.Store (_, _) -> "Store"
  | Ir.Load (_, _) -> "Load"
  | Ir.Alloc _ -> "Alloc"
  | Ir.Call (_, _, _) -> "Call"
  | _ -> "Other"

let dump_instrs title instrs =
  Printf.eprintf "=== %s ===\n" title;
  List.iter (fun i -> Printf.eprintf "%s\n" (string_of_instr i)) instrs;
  Printf.eprintf "=== end %s ===\n" title

let optimize_func (func : Ir.ir_func) : Ir.ir_func =
  let cfg = Cfg_builder.build_cfg func in
  let linear_after_build = Cfg.cfg_to_linear cfg in
  dump_instrs "After CFG build" linear_after_build;

  let optimized_cfg = optimize_cfg cfg in
  let linear_after_opt = Cfg.cfg_to_linear optimized_cfg in
  dump_instrs "After optimization" linear_after_opt;
  { func with body = linear_after_opt }