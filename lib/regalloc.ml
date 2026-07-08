(** 寄存器分配（Step 2 暂不分配，直接传递） *)

type alloc_function = {
  name   : string;
  instrs : Riscv.mach_instr list;
}

let allocate_registers (mfunc : Select.machine_func) : alloc_function =
  { name   = mfunc.name;
    instrs = mfunc.instrs }