(** 寄存器分配 - 基于活跃变量分析的线性扫描算法 *)

open Riscv

type alloc_function = {
  name   : string;
  instrs : mach_instr list;
}

(** 可用的物理寄存器池 *)
let temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"]
let saved_regs = ["s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]
let available_regs = temp_regs @ saved_regs  (* 19个寄存器 *)

(** 全局映射表 *)
let reg_map : (int, string) Hashtbl.t = Hashtbl.create 128
let spill_slots : (int, int) Hashtbl.t = Hashtbl.create 128
let spill_slot_count : int ref = ref 0

(** 初始化分配器 *)
let init_allocator () =
  Hashtbl.clear reg_map;
  Hashtbl.clear spill_slots;
  spill_slot_count := 0

(** 活跃区间数据结构 *)
type live_interval = {
  vreg : int;
  start_pos : int;
  end_pos : int;
  mutable assigned_reg : string option;
}

(** ========== 2. 辅助函数 ========== *)

(** 从 register 提取 vreg id *)
let get_vreg_id = function
  | VReg id -> id
  | PhysReg _ -> failwith "get_vreg_id: not a virtual register"

(** 检查 vreg 是否溢出（未分配到物理寄存器） *)
let is_spilled vreg =
  not (Hashtbl.mem reg_map vreg)

(** 从 register 提取 vreg id option *)
let get_vreg_from_register = function
  | VReg id -> Some id
  | PhysReg _ -> None

(** 获取指令中使用和定义的虚拟寄存器 *)
let get_used_and_defined_vregs (instr : mach_instr) : (int list * int list) =
  let get_vreg reg = 
    match get_vreg_from_register reg with
    | Some id -> [id]
    | None -> []
  in
  
  match instr with
  | Label _ | FrameSetup _ | FrameTeardown _ | MRet | J _ | Call _ -> ([], [])
  
  | Addi (rd, rs, _) -> (get_vreg rs, get_vreg rd)
  | Li (rd, _) -> ([], get_vreg rd)
  | La (rd, _) -> ([], get_vreg rd)
  | Mv (rd, rs) -> (get_vreg rs, get_vreg rd)
  
  | Lw (rd, _, rs) -> (get_vreg rs, get_vreg rd)
  | Sw (rs, _, rd) -> (get_vreg rs @ get_vreg rd, [])
  
  | Add (rd, rs1, rs2) | Sub (rd, rs1, rs2) | Mul (rd, rs1, rs2)
  | Div (rd, rs1, rs2) | Rem (rd, rs1, rs2)
  | Slt (rd, rs1, rs2) | Sle (rd, rs1, rs2) | Sgt (rd, rs1, rs2)
  | Sge (rd, rs1, rs2) | Seq (rd, rs1, rs2) | Sne (rd, rs1, rs2) ->
      (get_vreg rs1 @ get_vreg rs2, get_vreg rd)
  
  | Neg (rd, rs) | Seqz (rd, rs) | Snez (rd, rs) ->
      (get_vreg rs, get_vreg rd)
  
  | Beqz (rs, _) | Bnez (rs, _) -> (get_vreg rs, [])

(** 临时寄存器分配器（用于溢出时的 load/store） *)
let temp_counter = ref 0
let fresh_temp () =
  let reg = PhysReg (List.nth temp_regs (!temp_counter mod List.length temp_regs)) in
  incr temp_counter;
  reg

(** ========== 3. 活跃区间计算 ========== *)

let compute_live_intervals (instrs : mach_instr list) : live_interval list =
  let first_use = Hashtbl.create 128 in
  let last_use = Hashtbl.create 128 in
  
  List.iteri (fun pos instr ->
    let (used, defined) = get_used_and_defined_vregs instr in
    
    List.iter (fun vreg ->
      if not (Hashtbl.mem first_use vreg) then
        Hashtbl.add first_use vreg pos;
      Hashtbl.replace last_use vreg pos
    ) used;
    
    List.iter (fun vreg ->
      if not (Hashtbl.mem first_use vreg) then
        Hashtbl.add first_use vreg pos;
      Hashtbl.replace last_use vreg pos
    ) defined
  ) instrs;
  
  let intervals = ref [] in
  Hashtbl.iter (fun vreg start ->
    let end_pos = Hashtbl.find last_use vreg in
    intervals := { vreg; start_pos = start; end_pos; assigned_reg = None } :: !intervals
  ) first_use;
  
  List.sort (fun a b -> compare a.start_pos b.start_pos) !intervals

(** ========== 4. 线性扫描核心 ========== *)

let linear_scan_allocation (intervals : live_interval list) : unit =
  let active = ref [] in
  let free_regs = ref (List.mapi (fun i r -> (i, r)) available_regs) in
  
  let expire_old_intervals current_pos =
    let (still_active, expired) = 
      List.partition (fun interval -> interval.end_pos >= current_pos) !active 
    in
    active := still_active;
    
    List.iter (fun interval ->
      match interval.assigned_reg with
      | Some reg -> 
          let idx = 
            List.mapi (fun i r -> (i, r)) available_regs
            |> List.find (fun (_, r) -> r = reg)
            |> fst
          in
          free_regs := (idx, reg) :: !free_regs
      | None -> ()
    ) expired;
    
    free_regs := List.sort (fun (i1, _) (i2, _) -> compare i1 i2) !free_regs
  in
  
  let spill_at_interval interval =
    if !active = [] then
      interval.assigned_reg <- None
    else
      let spill_candidate = 
        List.fold_left (fun acc i ->
          if i.end_pos > acc.end_pos then i else acc
        ) (List.hd !active) (List.tl !active)
      in
      
      if spill_candidate.end_pos > interval.end_pos then begin
        interval.assigned_reg <- spill_candidate.assigned_reg;
        spill_candidate.assigned_reg <- None;
        active := interval :: List.filter (fun i -> i.vreg <> spill_candidate.vreg) !active;
        active := List.sort (fun a b -> compare a.end_pos b.end_pos) !active
      end else
        interval.assigned_reg <- None
  in
  
  List.iter (fun interval ->
    expire_old_intervals interval.start_pos;
    
    if List.length !active >= List.length available_regs then
      spill_at_interval interval
    else begin
      match !free_regs with
      | (_, reg) :: rest ->
          free_regs := rest;
          interval.assigned_reg <- Some reg;
          active := interval :: !active;
          active := List.sort (fun a b -> compare a.end_pos b.end_pos) !active
      | [] -> 
          spill_at_interval interval
    end
  ) intervals;
  
  (* 将成功分配的结果写入 reg_map *)
  List.iter (fun interval ->
    match interval.assigned_reg with
    | Some reg -> Hashtbl.add reg_map interval.vreg reg
    | None -> () (* 溢出的寄存器不加入 reg_map *)
  ) intervals

(** ========== 5. 溢出槽管理 ========== *)

let get_spill_slot vreg =
  try
    Hashtbl.find spill_slots vreg
  with Not_found ->
    let offset = !spill_slot_count in
    spill_slot_count := !spill_slot_count + 4;
    Hashtbl.add spill_slots vreg offset;
    offset

(** ========== 6. 指令重写 ========== *)

let apply_allocation (instrs : mach_instr list) (spill_size : int) : mach_instr list =
  temp_counter := 0;
  
  (* 映射寄存器：如果溢出则返回临时寄存器和加载指令 *)
  let map_register_for_use reg =
    match reg with
    | PhysReg name -> (PhysReg name, [])
    | VReg id ->
        if is_spilled id then
          let temp = fresh_temp () in
          let offset = -(56 + spill_size - get_spill_slot id) in
          (temp, [Lw (temp, offset, PhysReg "fp")])
        else
          (PhysReg (Hashtbl.find reg_map id), [])
  in
  
  (* 映射寄存器：如果溢出则返回临时寄存器和存储指令 *)
  let map_register_for_def reg =
    match reg with
    | PhysReg name -> (PhysReg name, [])
    | VReg id ->
        if is_spilled id then
          let temp = fresh_temp () in
          let offset = -(56 + spill_size - get_spill_slot id) in
          (temp, [Sw (temp, offset, PhysReg "fp")])
        else
          (PhysReg (Hashtbl.find reg_map id), [])
  in
  
  let transform_instr = function
    | Label l -> [Label l]
    | FrameSetup n -> [FrameSetup (n + spill_size)]
    | FrameTeardown n -> [FrameTeardown (n + spill_size)]
    | MRet -> [MRet]
    | J l -> [J l]
    | Call f -> [Call f]
    
    | Li (rd, imm) ->
        let (rd', store_instrs) = map_register_for_def rd in
        [Li (rd', imm)] @ store_instrs
    
    | La (rd, sym) ->
        let (rd', store_instrs) = map_register_for_def rd in
        [La (rd', sym)] @ store_instrs
    
    | Addi (rd, rs, imm) ->
        let (rs', load_instrs) = map_register_for_use rs in
        let (rd', store_instrs) = map_register_for_def rd in
        load_instrs @ [Addi (rd', rs', imm)] @ store_instrs
    
    | Mv (rd, rs) ->
        let (rs', load_instrs) = map_register_for_use rs in
        let (rd', store_instrs) = map_register_for_def rd in
        load_instrs @ [Mv (rd', rs')] @ store_instrs
    
    | Lw (rd, offset, rs) ->
        let (rs', load_instrs) = map_register_for_use rs in
        let (rd', store_instrs) = map_register_for_def rd in
        load_instrs @ [Lw (rd', offset, rs')] @ store_instrs
    
    | Sw (rs, offset, rd) ->
        let (rs', load_rs) = map_register_for_use rs in
        let (rd', load_rd) = map_register_for_use rd in
        load_rs @ load_rd @ [Sw (rs', offset, rd')]
    
    | Add (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Add (rd', rs1', rs2')] @ store
    
    | Sub (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Sub (rd', rs1', rs2')] @ store
    
    | Mul (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Mul (rd', rs1', rs2')] @ store
    
    | Div (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Div (rd', rs1', rs2')] @ store
    
    | Rem (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Rem (rd', rs1', rs2')] @ store
    
    | Neg (rd, rs) ->
        let (rs', load) = map_register_for_use rs in
        let (rd', store) = map_register_for_def rd in
        load @ [Neg (rd', rs')] @ store
    
    | Seqz (rd, rs) ->
        let (rs', load) = map_register_for_use rs in
        let (rd', store) = map_register_for_def rd in
        load @ [Seqz (rd', rs')] @ store
    
    | Snez (rd, rs) ->
        let (rs', load) = map_register_for_use rs in
        let (rd', store) = map_register_for_def rd in
        load @ [Snez (rd', rs')] @ store
    
    | Slt (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Slt (rd', rs1', rs2')] @ store
    
    | Sle (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Sle (rd', rs1', rs2')] @ store
    
    | Sgt (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Sgt (rd', rs1', rs2')] @ store
    
    | Sge (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Sge (rd', rs1', rs2')] @ store
    
    | Seq (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Seq (rd', rs1', rs2')] @ store
    
    | Sne (rd, rs1, rs2) ->
        let (rs1', load1) = map_register_for_use rs1 in
        let (rs2', load2) = map_register_for_use rs2 in
        let (rd', store) = map_register_for_def rd in
        load1 @ load2 @ [Sne (rd', rs1', rs2')] @ store
    
    | Beqz (rs, lbl) ->
        let (rs', load) = map_register_for_use rs in
        load @ [Beqz (rs', lbl)]
    
    | Bnez (rs, lbl) ->
        let (rs', load) = map_register_for_use rs in
        load @ [Bnez (rs', lbl)]
  in
  
  List.concat_map transform_instr instrs

(** ========== 7. 主入口函数 ========== *)

let allocate_registers (mfunc : Select.machine_func) : alloc_function =
  (* 初始化 *)
  init_allocator ();
  
  (* 计算活跃区间 *)
  let intervals = compute_live_intervals mfunc.instrs in
  
  (* 执行线性扫描分配 *)
  linear_scan_allocation intervals;
  
  (* 为溢出的区间分配栈槽 *)
  List.iter (fun interval ->
    if interval.assigned_reg = None then
      ignore (get_spill_slot interval.vreg)
  ) intervals;
  
  (* 应用分配结果，重写指令 *)
  let new_instrs = apply_allocation mfunc.instrs !spill_slot_count in
  
  { name = mfunc.name; instrs = new_instrs }