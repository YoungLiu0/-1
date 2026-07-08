(*它的输入是 Ir.insn 列表（纯 IR，如 Add("t1", "t2", "t3")）
输出是 Riscv.insn 列表（但操作数仍然是“虚拟寄存器”）。
关键点：它只做“指令模板匹配”，不碰寄存器分配！*)
(** 指令选择：将高级 IR 翻译为机器 IR *)

open Ir
open Riscv

type machine_func = {
  name   : string;
  instrs : mach_instr list;
}

let select_function (func : ir_func) (_cfg : Cfg_builder.cfg) : machine_func =
  let select_instr = function
    | Ret (Some n) -> [Li (PhysReg "a0", n); MRet]
    | Ret None     -> [MRet]
    | _ -> failwith "Step 1: unexpected IR instruction"
  in
  let body_instrs = List.concat_map select_instr func.body in
  { name   = func.name;
    instrs = Label func.name :: body_instrs }