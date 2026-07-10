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

(** 虚拟寄存器到物理寄存器的映射 *)
let reg_map : (int, string) Hashtbl.t = Hashtbl.create 128

(** 初始化分配器 *)
let init_allocator () =
  Hashtbl.clear reg_map

(** 简单贪心分配 - fallback 策略 *)
let simple_greedy_allocation (instrs : mach_instr list) : mach_instr list =
  let next_reg = ref 0 in
  
  let allocate_vreg id =
    try
      Hashtbl.find reg_map id
    with Not_found ->
      if !next_reg >= List.length available_regs then
        List.nth available_regs (id mod List.length available_regs)  (* 循环使用 *)
      else begin
        let reg = List.nth available_regs !next_reg in
        incr next_reg;
        Hashtbl.add reg_map id reg;
        reg
      end
  in
  
  let map_register = function
    | PhysReg name -> PhysReg name
    | VReg id -> PhysReg (allocate_vreg id)
  in
  
  let transform_instr = function
    | Label l -> Label l
    | FrameSetup n -> FrameSetup n
    | FrameTeardown n -> FrameTeardown n
    | MRet -> MRet
    | J l -> J l
    | Call f -> Call f
     | Addi (rd, rs, imm) -> Addi (map_register rd, map_register rs, imm)
    | Li (rd, imm) -> Li (map_register rd, imm)
    | La (rd, sym) -> La (map_register rd, sym)
    | Mv (rd, rs) -> Mv (map_register rd, map_register rs)
    
    | Lw (rd, offset, rs) -> Lw (map_register rd, offset, map_register rs)
    | Sw (rs, offset, rd) -> Sw (map_register rs, offset, map_register rd)
    
    | Add (rd, rs1, rs2) -> Add (map_register rd, map_register rs1, map_register rs2)
    | Sub (rd, rs1, rs2) -> Sub (map_register rd, map_register rs1, map_register rs2)
    | Mul (rd, rs1, rs2) -> Mul (map_register rd, map_register rs1, map_register rs2)
    | Div (rd, rs1, rs2) -> Div (map_register rd, map_register rs1, map_register rs2)
    | Rem (rd, rs1, rs2) -> Rem (map_register rd, map_register rs1, map_register rs2)
    
    | Neg (rd, rs) -> Neg (map_register rd, map_register rs)
    | Seqz (rd, rs) -> Seqz (map_register rd, map_register rs)
    | Snez (rd, rs) -> Snez (map_register rd, map_register rs)
    
    | Slt (rd, rs1, rs2) -> Slt (map_register rd, map_register rs1, map_register rs2)
    | Sle (rd, rs1, rs2) -> Sle (map_register rd, map_register rs1, map_register rs2)
    | Sgt (rd, rs1, rs2) -> Sgt (map_register rd, map_register rs1, map_register rs2)
    | Sge (rd, rs1, rs2) -> Sge (map_register rd, map_register rs1, map_register rs2)
    | Seq (rd, rs1, rs2) -> Seq (map_register rd, map_register rs1, map_register rs2)
    | Sne (rd, rs1, rs2) -> Sne (map_register rd, map_register rs1, map_register rs2)
    
    | Beqz (rs, lbl) -> Beqz (map_register rs, lbl)
    | Bnez (rs, lbl) -> Bnez (map_register rs, lbl)
  in
  
  List.map transform_instr instrs

(** 主分配函数 *)
let allocate_registers (mfunc : Select.machine_func) : alloc_function =
  init_allocator ();
  let new_instrs = simple_greedy_allocation mfunc.instrs in
  { name = mfunc.name; instrs = new_instrs }