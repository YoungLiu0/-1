(*定义中间表示*)
type operand = Imm of int32|Reg of int|Global of string

type op =

type unop=

type instr=


type func =

type program=

(*==============辅助生成函数====================*)
let next_reg = ref 0;;
let next_label= ref 0
(*===============AST->IR转换模块==========*)
module Translate=struct
  open Ast
  type env = {
  vars    : (string * operand) list;   (* 局部变量/形参 → 虚拟寄存器 *)
  consts  : (string * int32) list;     (* 常量名 → 编译期已知的立即数值 *)
  funcs   : (string * Ir.func) list;   (* 函数名 → IR 函数对象，或者至少记录其标签 *)
  }
  (*全局变量声明集成到环境和main函数里面*)

end
(*辅助函数，打印IR*)