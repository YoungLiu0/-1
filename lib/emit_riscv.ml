(** 汇编发射：将机器 IR 转为 RISC-V 汇编文本 *)

open Riscv

let reg_to_string = function
  | PhysReg name -> name
  | VReg n -> Printf.sprintf "t%d" (n mod 7)

let emit_function (func : Regalloc.alloc_function) : string =
  let buf = Buffer.create 512 in
  
  List.iter (fun instr ->
    match instr with
    | Label name -> Buffer.add_string buf (name ^ ":\n")
    
    | FrameSetup size ->
        Buffer.add_string buf (Printf.sprintf "  addi sp, sp, -%d\n" size);
        Buffer.add_string buf (Printf.sprintf "  sw ra, %d(sp)\n" (size - 4));
        Buffer.add_string buf (Printf.sprintf "  sw fp, %d(sp)\n" (size - 8));
        Buffer.add_string buf "  addi fp, sp, 0\n"
    
    | FrameTeardown size ->
        Buffer.add_string buf (Printf.sprintf "  lw ra, %d(sp)\n" (size - 4));
        Buffer.add_string buf (Printf.sprintf "  lw fp, %d(sp)\n" (size - 8));
        Buffer.add_string buf (Printf.sprintf "  addi sp, sp, %d\n" size)
    
    | Li (rd, imm) ->
        Buffer.add_string buf (Printf.sprintf "  li %s, %d\n" (reg_to_string rd) imm)
    
    | Lw (rd, offset, rs) ->
        Buffer.add_string buf (Printf.sprintf "  lw %s, %d(%s)\n" 
          (reg_to_string rd) offset (reg_to_string rs))
    
    | Sw (rs, offset, rd) ->
        Buffer.add_string buf (Printf.sprintf "  sw %s, %d(%s)\n"
          (reg_to_string rs) offset (reg_to_string rd))
    
    | Mv (rd, rs) ->
        Buffer.add_string buf (Printf.sprintf "  mv %s, %s\n" 
          (reg_to_string rd) (reg_to_string rs))
    
    | Add (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  add %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Sub (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  sub %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Mul (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  mul %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Div (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  div %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Rem (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  rem %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Neg (rd, rs) ->
        Buffer.add_string buf (Printf.sprintf "  neg %s, %s\n"
          (reg_to_string rd) (reg_to_string rs))
    
    | Seqz (rd, rs) ->
        Buffer.add_string buf (Printf.sprintf "  seqz %s, %s\n"
          (reg_to_string rd) (reg_to_string rs))
    
    | Slt (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  slt %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | MRet -> Buffer.add_string buf "  ret\n"
    
    | _ -> ()
  ) func.instrs;
  
  Buffer.contents buf

let emit_program (funcs : Regalloc.alloc_function list) : string =
  "  .text\n  .globl main\n" ^
  String.concat "" (List.map emit_function funcs)