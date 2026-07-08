(*emit_riscv.ml 只负责打印文本（把 Riscv.insn 结构体变成 .s 字符串）*)
(** 汇编发射：将机器 IR 转为 RISC-V 汇编文本 *)

open Riscv

let emit_function (func : Regalloc.alloc_function) : string =
  let buf = Buffer.create 128 in
  List.iter (fun instr ->
    match instr with
    | Label name -> Buffer.add_string buf (name ^ ":\n")
    | Li (PhysReg rd, imm) ->
        Buffer.add_string buf (Printf.sprintf "  li %s, %d\n" rd imm)
    | MRet -> Buffer.add_string buf "  ret\n"
    | _ -> failwith "Step 1: unexpected machine instruction"
  ) func.instrs;
  Buffer.contents buf

let emit_program (funcs : Regalloc.alloc_function list) : string =
  ".text\n  .globl main\n" ^
  String.concat "" (List.map emit_function funcs)