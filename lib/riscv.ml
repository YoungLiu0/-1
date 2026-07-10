(** 机器级中间表示 (Machine IR) 定义 *)

type mach_reg =
  | PhysReg of string
  | VReg of int

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
  | Snez of mach_reg * mach_reg
  (* 比较指令 *)
  | Slt of mach_reg * mach_reg * mach_reg
  | Sle of mach_reg * mach_reg * mach_reg
  | Sgt of mach_reg * mach_reg * mach_reg
  | Sge of mach_reg * mach_reg * mach_reg
  | Seq of mach_reg * mach_reg * mach_reg
  | Sne of mach_reg * mach_reg * mach_reg
  (* 内存操作 *)
  | Lw of mach_reg * int * mach_reg
  | Sw of mach_reg * int * mach_reg
  (* 跳转指令 *)
  | J of string
  | Beqz of mach_reg * string
  | Bnez of mach_reg * string
  (* Step 5 新增：函数调用和全局变量 *)
  | Call of string                    (* call func_name *)
  | La of mach_reg * string           (* la rd, symbol *)
  (* 栈操作 *)
  | FrameSetup of int
  | FrameTeardown of int
  | MRet