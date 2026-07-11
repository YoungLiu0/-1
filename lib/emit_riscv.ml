(** 汇编发射：将机器 IR 转为 RISC-V 汇编文本 *)

open Riscv
open Regalloc
let reg_to_string = function
  | PhysReg name -> name
  | VReg n -> Printf.sprintf "t%d" (n mod 7)
let string_contains (s : string) (sub : string) : bool =
  let len_s = String.length s in
  let len_sub = String.length sub in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0

let emit_function (func : Regalloc.alloc_function) : string =
  let buf = Buffer.create 1024 in
  let prefix = func.name ^ "."in
  let label name = prefix ^ name in
  List.iter (fun instr ->
    match instr with
       | Label name ->
         Buffer.add_string buf (Printf.sprintf "%s:\n" (label name))
    
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
    
    | Snez (rd, rs) ->
        Buffer.add_string buf (Printf.sprintf "  snez %s, %s\n"
          (reg_to_string rd) (reg_to_string rs))
    
    | Slt (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  slt %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs1) (reg_to_string rs2))
    
    | Sle (rd, rs1, rs2) ->
        let tmp = VReg 250 in
        Buffer.add_string buf (Printf.sprintf "  slt %s, %s, %s\n"
          (reg_to_string tmp) (reg_to_string rs2) (reg_to_string rs1));
        Buffer.add_string buf (Printf.sprintf "  xori %s, %s, 1\n"
          (reg_to_string rd) (reg_to_string tmp))
    
    | Sgt (rd, rs1, rs2) ->
        Buffer.add_string buf (Printf.sprintf "  slt %s, %s, %s\n"
          (reg_to_string rd) (reg_to_string rs2) (reg_to_string rs1))
    
    | Sge (rd, rs1, rs2) ->
        let tmp = VReg 251 in
        Buffer.add_string buf (Printf.sprintf "  slt %s, %s, %s\n"
          (reg_to_string tmp) (reg_to_string rs1) (reg_to_string rs2));
        Buffer.add_string buf (Printf.sprintf "  xori %s, %s, 1\n"
          (reg_to_string rd) (reg_to_string tmp))
    
    | Seq (rd, rs1, rs2) ->
        let tmp = VReg 252 in
        Buffer.add_string buf (Printf.sprintf "  sub %s, %s, %s\n"
          (reg_to_string tmp) (reg_to_string rs1) (reg_to_string rs2));
        Buffer.add_string buf (Printf.sprintf "  seqz %s, %s\n"
          (reg_to_string rd) (reg_to_string tmp))
    
    | Sne (rd, rs1, rs2) ->
        let tmp = VReg 253 in
        Buffer.add_string buf (Printf.sprintf "  sub %s, %s, %s\n"
          (reg_to_string tmp) (reg_to_string rs1) (reg_to_string rs2));
        Buffer.add_string buf (Printf.sprintf "  snez %s, %s\n"
          (reg_to_string rd) (reg_to_string tmp))
    
    | J lbl ->
        Buffer.add_string buf (Printf.sprintf "  j %s\n" (label lbl))
    
    | Beqz (rs, lbl) ->
        Buffer.add_string buf (Printf.sprintf "  beqz %s, %s\n" (reg_to_string rs) (label lbl))
    
    | Bnez (rs, lbl) ->
        Buffer.add_string buf (Printf.sprintf "  bnez %s, %s\n" (reg_to_string rs) (label lbl))
    
    (* Step 5 新增指令 *)
    | Call func_name ->
        Buffer.add_string buf (Printf.sprintf "  call %s\n" func_name)
    
    | La (rd, symbol) ->
        Buffer.add_string buf (Printf.sprintf "  la %s, %s\n" (reg_to_string rd) symbol)
    
    | MRet -> Buffer.add_string buf "  ret\n"
    
    | _ -> ()
  ) func.instrs;
  
  Buffer.contents buf

(* 输出全局变量的 .data 段 *)
let emit_global_var (g : Ir.ir_global) : string =
  let init_val = match g.g_init with
    | Some n -> string_of_int n
    | None -> "0"
  in
  Printf.sprintf "  .globl %s\n%s:\n  .word %s\n" g.g_name g.g_name init_val

(* 修改后的 emit_program：接受全局变量列表和函数列表 *)
let emit_program (globals : Ir.ir_global list) (funcs : Regalloc.alloc_function list) : string =
  let data_section =
    if List.length globals > 0 then
      "  .data\n" ^ String.concat "" (List.map emit_global_var globals) ^ "\n"
    else
      ""
  in
   let text_section =
    "  .text\n" ^
    String.concat "\n" (List.map (fun func ->
      (* 给每个函数的入口标签加 .globl 声明 *)
      Printf.sprintf "  .globl %s\n%s" func.name (emit_function func)
    ) funcs)
  in
  data_section ^ text_section