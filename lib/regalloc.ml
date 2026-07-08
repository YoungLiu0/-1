(* open Riscv *)
(*使用优化型线性扫描*)
(** 寄存器分配（Step 1 不做实际分配，直接返回） *)

open Riscv

type alloc_function = {
  name   : string;
  instrs : mach_instr list;
}

let allocate_registers (mfunc : Select.machine_func) : alloc_function =
  { name   = mfunc.name;
    instrs = mfunc.instrs }          (* 直接传递 *)