(** 机器级中间表示 (Machine IR) 定义 *)

type mach_reg =
  | PhysReg of string    (* 物理寄存器 *)
  | VReg of int          (* 虚拟寄存器 *)

type mach_instr =
  | Label of string
  | Li of mach_reg * int
  | Mv of mach_reg * mach_reg
  | Add of mach_reg * mach_reg * mach_reg
  | Sub of mach_reg * mach_reg * mach_reg
  | Mul of mach_reg * mach_reg * mach_reg
  | Div of mach_reg * mach_reg * mach_reg
  | Rem of mach_reg * mach_reg * mach_reg
  | Addi of mach_reg * mach_reg * int
  | Neg of mach_reg * mach_reg
  | Seqz of mach_reg * mach_reg
  | Slt of mach_reg * mach_reg * mach_reg
  (* 内存操作 *)
  | Lw of mach_reg * int * mach_reg       (* lw rd, offset(rs) *)
  | Sw of mach_reg * int * mach_reg       (* sw rs, offset(rd) *)
  (* 栈操作 *)
  | FrameSetup of int                     (* 设置栈帧 *)
  | FrameTeardown of int                  (* 恢复栈帧 *)
  | MRet