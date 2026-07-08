(** 指令选择：将高级 IR 翻译为机器 IR *)

open Ir
open Riscv

type machine_func = {
  name   : string;
  instrs : mach_instr list;
  frame_size : int;  (* 栈帧大小 *)
}

(* 变量到栈偏移的映射 *)
let var_offset_map : (string, int) Hashtbl.t = Hashtbl.create 16

let compute_frame_layout (locals : string list) : int =
  Hashtbl.clear var_offset_map;
  let offset = ref 0 in
  List.iter (fun var ->
    offset := !offset + 4;
    Hashtbl.add var_offset_map var (!offset)
  ) locals;
  !offset

let get_var_offset var =
  try -(Hashtbl.find var_offset_map var)
  with Not_found -> failwith ("Variable not in frame: " ^ var)

let operand_to_reg = function
  | Imm _ -> failwith "Cannot convert immediate to register directly"
  | Temp t -> VReg t
  | Param _ -> PhysReg ("a" ^ string_of_int 0)  (* 简化：只支持一个参数 *)
  | Local _ -> failwith "Local should not be converted to register directly"

let select_function (func : ir_func) (_cfg : Cfg_builder.cfg) : machine_func =
  let frame_size = compute_frame_layout func.locals in
  let frame_aligned = ((frame_size + 15) / 16) * 16 in  (* 16字节对齐 *)
  
  let select_instr = function
    | Ret (Some (Imm n)) ->
        [Li (PhysReg "a0", n); FrameTeardown frame_aligned; MRet]
    
    | Ret (Some op) ->
        let reg = operand_to_reg op in
        [Mv (PhysReg "a0", reg); FrameTeardown frame_aligned; MRet]
    
    | Ret None -> [FrameTeardown frame_aligned; MRet]
    
    | Alloc _ -> []  (* 栈空间已在 FrameSetup 中分配 *)
    
    | Store (var, Imm n) ->
        let offset = get_var_offset var in
        let tmp = VReg 200 in
        [Li (tmp, n); Sw (tmp, offset, PhysReg "fp")]
    
    | Store (var, op) ->
        let offset = get_var_offset var in
        let reg = operand_to_reg op in
        [Sw (reg, offset, PhysReg "fp")]
    
    | Load (dest, var) ->
        let offset = get_var_offset var in
        let rd = operand_to_reg dest in
        [Lw (rd, offset, PhysReg "fp")]
    
    | Move (dest, Imm n) ->
        let rd = operand_to_reg dest in
        [Li (rd, n)]
    
    | Move (dest, src) ->
        let rd = operand_to_reg dest in
        let rs = operand_to_reg src in
        [Mv (rd, rs)]
    
    | BinOp (dest, op, op1, op2) ->
        let rd = operand_to_reg dest in
        let load_operand operand tmp_reg =
          match operand with
          | Imm n -> ([Li (tmp_reg, n)], tmp_reg)
          | _ -> ([], operand_to_reg operand)
        in
        let (instrs1, r1) = load_operand op1 (VReg 100) in
        let (instrs2, r2) = load_operand op2 (VReg 101) in
        let op_instr = match op with
          | Ast.Add -> Add (rd, r1, r2)
          | Ast.Sub -> Sub (rd, r1, r2)
          | Ast.Mul -> Mul (rd, r1, r2)
          | Ast.Div -> Div (rd, r1, r2)
          | Ast.Mod -> Rem (rd, r1, r2)
          | Ast.Lt -> Slt (rd, r1, r2)
          | _ -> failwith "Step 3: unsupported binary operator"
        in
        instrs1 @ instrs2 @ [op_instr]
    
    | UnaryOp (dest, op, operand) ->
        let rd = operand_to_reg dest in
        let (load_instrs, rs) = match operand with
          | Imm n -> ([Li (VReg 102, n)], VReg 102)
          | _ -> ([], operand_to_reg operand)
        in
        let op_instr = match op with
          | Ast.Neg -> Neg (rd, rs)
          | Ast.Not -> Seqz (rd, rs)
          | Ast.Pos -> Mv (rd, rs)
        in
        load_instrs @ [op_instr]
    
    | Label lbl -> [Label lbl]
  in
  
  let prologue = [FrameSetup frame_aligned] in
  let body_instrs = List.concat_map select_instr func.body in
  
  { name = func.name;
    instrs = Label func.name :: prologue @ body_instrs;
    frame_size = frame_aligned }