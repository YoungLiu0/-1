(* lib/loop_var_cache.ml *)
open Ir
open LoopInfo

(* ---- 辅助函数 ---- *)
let find_label_index label instrs =
  let rec aux i = function
    | [] -> None
    | Label l :: _ when l = label -> Some i
    | _ :: rest -> aux (i+1) rest
  in aux 0 instrs

let has_call_instrs instrs =
  List.exists (function Call _ -> true | _ -> false) instrs

(* ---- 对单个叶子循环进行变量缓存 ---- *)
let cache_one_loop (loop : loop_info) (instrs : ir_instr list) : ir_instr list =
  let hdr  = loop.header in
  let exit = loop.exit in
  let hdr_idx = match find_label_index hdr instrs with Some i -> i | None -> failwith "header missing" in
  let exit_idx = match find_label_index exit instrs with Some i -> i | None -> failwith "exit missing" in

  (* 循环范围指令（含 header 标签和 exit 标签） *)
  let loop_range = List.filteri (fun i _ -> i >= hdr_idx && i <= exit_idx) instrs in
  if has_call_instrs loop_range then instrs   (* 循环内有函数调用，放弃 *)
  else begin
    (* 收集循环内被修改的局部变量（出现 Store 的变量） *)
    let modified_vars = Hashtbl.create 8 in
    List.iter (function
      | Store (v, _) -> Hashtbl.replace modified_vars v true
      | _ -> ()
    ) loop_range;

    if Hashtbl.length modified_vars = 0 then instrs
    else begin
      (* 为每个被修改变量生成一个新的临时寄存器 *)
      let var_to_reg = Hashtbl.create 8 in
      Hashtbl.iter (fun v _ ->
        Hashtbl.add var_to_reg v (fresh_temp ())
      ) modified_vars;

      (* 找到循环前每个变量的最新值（最后一次 Store），作为寄存器的初始值 *)
      let prefix = List.filteri (fun i _ -> i < hdr_idx) instrs in
      let var_to_init = Hashtbl.create 8 in
      List.iter (function
        | Store (v, src) -> Hashtbl.replace var_to_init v src
        | _ -> ()
      ) (List.rev prefix);   (* 逆序遍历，后来的 Store 会覆盖之前的 *)

      (* 构建新指令列表 *)
      let new_instrs = ref [] in

      (* 1. 前缀部分（header 标签之前） *)
      let prefix_instrs = List.filteri (fun i _ -> i < hdr_idx) instrs in
      new_instrs := prefix_instrs;

      (* 2. 在 header 标签前插入 Move 指令，将变量初始值加载到寄存器 *)
      let pre_loads = Hashtbl.fold (fun v _ acc ->
        let reg = Hashtbl.find var_to_reg v in
        let init_src = try Hashtbl.find var_to_init v with Not_found -> Imm 0 in
        (Move (reg, init_src)) :: acc
      ) modified_vars [] in
      new_instrs := !new_instrs @ pre_loads;

      (* 3. 处理循环体指令（从 header 到 exit 之前） *)
      let body_instrs = List.filteri (fun i _ -> i >= hdr_idx && i < exit_idx) instrs in
      let processed_body = List.map (function
        | Load (dest, v) when Hashtbl.mem var_to_reg v ->
            Move (dest, Hashtbl.find var_to_reg v)            (* 读操作 → 寄存器 move *)
        | Store (v, src) when Hashtbl.mem var_to_reg v ->
            Move (Hashtbl.find var_to_reg v, src)             (* 写操作 → 写入寄存器 *)
        | other -> other
      ) body_instrs in
      new_instrs := !new_instrs @ processed_body;

      (* 4. exit 标签和后续指令 *)
      let suffix = List.filteri (fun i _ -> i >= exit_idx) instrs in
      (* 在 exit 标签之后立即插入 Store，将寄存器值写回栈 *)
      let exit_label_instr = Label exit in
      let post_stores = Hashtbl.fold (fun v _ acc ->
        let reg = Hashtbl.find var_to_reg v in
        (Store (v, reg)) :: acc
      ) modified_vars [] in
      (* 注意：suffix 的第一个元素就是 Label exit，我们需要将 stores 插入其后 *)
      let modified_suffix = match suffix with
        | (Label _) :: rest -> exit_label_instr :: post_stores @ rest
        | _ -> failwith "exit label not found"
      in
      new_instrs := !new_instrs @ modified_suffix;

      !new_instrs
    end
  end

(* ---- 处理所有叶子循环 ---- *)
let cache_loop_variables (instrs : ir_instr list) : ir_instr list =
  let loops = LoopInfo.all_loops () in
  if loops = [] then instrs
  else
    (* 筛选不含 break/continue 且不含嵌套循环的叶子循环 *)
    let loops_info = List.map (fun loop ->
      let hdr_idx = match find_label_index loop.header instrs with Some i -> i | None -> -1 in
      let exit_idx = match find_label_index loop.exit instrs with Some i -> i | None -> -1 in
      (loop, hdr_idx, exit_idx)
    ) loops in
    let leaf_loops = List.filter (fun (loop, h, e) ->
      h <> -1 && e <> -1 &&
      not !(loop.has_break_continue) &&
      (* 该循环区间内不包含其他循环的 header *)
      List.for_all (fun (_, h2, _) -> h2 <= h || h2 >= e) loops_info
    ) loops_info in
    (* 按 header 索引降序处理，防止前面的索引变动影响后面的循环 *)
    let leaf_loops = List.sort (fun (_,h1,_) (_,h2,_) -> compare h2 h1) leaf_loops in
    List.fold_left (fun acc (loop, _, _) -> cache_one_loop loop acc) instrs leaf_loops

(* ---- 对 CFG 进行优化（线性化 → 优化 → 重建 CFG） ---- *)
let cache_loop_vars_in_cfg (cfg : Cfg.t) : Cfg.t =
  let linear = Cfg.cfg_to_linear cfg in
  let new_linear = cache_loop_variables linear in
  let dummy_func = { name = ""; ret_type = Ast.Int; params = []; body = new_linear; locals = [] } in
  Cfg_builder.build_cfg dummy_func