(** 机器级中间表示 (Machine IR) 定义 *)

type mach_reg =
  | PhysReg of string    (* 物理寄存器，如 "a0", "sp" *)
  | VReg of int          (* 虚拟寄存器 *)

type mach_instr =
  | Li of mach_reg * int                 (* li rd, imm *)
  | MRet                                  (* ret *)
  | Label of string                       (* 汇编标签 *)
  (* 后续步骤可添加 add, lw, sw 等 *)