(** 指令选择：将高级 IR 翻译为机器 IR *)

open Ir
open Riscv

type machine_func = {
  name   : string;
  instrs : mach_instr list;
  frame_size : int;
}

type machine_program = {
  globals   : ir_global list;
  functions : machine_func list;
}

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
  | Param _ -> PhysReg "a0"
  | Local _ -> failwith "Local should not be converted to register directly"
  | Global _ -> failwith "Global should not be converted to register directly"


let select_function (func : ir_func) : machine_func =
  let locals_size = compute_frame_layout func.locals in
  let frame_size = max 8 (locals_size + 8) in
  let frame_aligned = ((frame_size + 15) / 16) * 16 in

  (* 临时寄存器计数器 *)
  let tmp_cnt = ref 2000 in
  let fresh_tmp () =
    let n = !tmp_cnt in
    incr tmp_cnt;
    VReg n
  in
  let select_instr = function
    | Ret (Some (Imm n)) ->
        [Li (PhysReg "a0", n); FrameTeardown frame_aligned; MRet]

    | Ret (Some op) ->
        let reg = operand_to_reg op in
        [Mv (PhysReg "a0", reg); FrameTeardown frame_aligned; MRet]

    | Ret None -> [FrameTeardown frame_aligned; MRet]

    | Alloc _ -> []

    | Store (var, Imm n) ->
        let offset = get_var_offset var in
        let tmp = fresh_tmp ()  in
        [Li (tmp, n); Sw (tmp, offset, PhysReg "fp")]

    | Store (var, op) ->
        let offset = get_var_offset var in
        let reg = operand_to_reg op in
        [Sw (reg, offset, PhysReg "fp")]

    | Load (dest, var) ->
        let offset = get_var_offset var in
        let rd = operand_to_reg dest in
        [Lw (rd, offset, PhysReg "fp")]

    (* 全局变量加载 *)
    | LoadGlobal (dest, var_name) ->
        let rd = operand_to_reg dest in
        let addr =fresh_tmp () in
        [La (addr, var_name); Lw (rd, 0, addr)]

    (* 全局变量存储 *)
    | StoreGlobal (var_name, src) ->
        let addr = fresh_tmp ()  in
        let (load_instrs, rs) = match src with
          | Imm n ->
              let tmp = VReg 201 in
              ([Li (tmp, n)], tmp)
          | _ -> ([], operand_to_reg src)
        in
        load_instrs @ [La (addr, var_name); Sw (rs, 0, addr)]

    (* 函数调用 *)
    | Call (dest, func_name, args) ->
        let arg_regs = ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"] in
        let move_args = List.mapi (fun i arg ->
          if i >= 8 then failwith "More than 8 arguments not supported yet";
          let target = PhysReg (List.nth arg_regs i) in
          match arg with
          | Imm n ->
              let tmp = VReg (100 + i) in
              [Li (tmp, n); Mv (target, tmp)]
          | _ -> [Mv (target, operand_to_reg arg)]
        ) args in
        let arg_instrs = List.concat move_args in
        let call_instr = [Call func_name] in
        let rd = operand_to_reg dest in
        let result_move = [Mv (rd, PhysReg "a0")] in
        arg_instrs @ call_instr @ result_move

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
        let (instrs1, r1) = load_operand op1 (fresh_tmp ()) in
        let (instrs2, r2) = load_operand op2 (fresh_tmp ()) in
        let op_instr = match op with
          | Ast.Add -> Add (rd, r1, r2)
          | Ast.Sub -> Sub (rd, r1, r2)
          | Ast.Mul -> Mul (rd, r1, r2)
          | Ast.Div -> Div (rd, r1, r2)
          | Ast.Mod -> Rem (rd, r1, r2)
          | Ast.Lt -> Slt (rd, r1, r2)
          | Ast.Le -> Sle (rd, r1, r2)
          | Ast.Gt -> Sgt (rd, r1, r2)
          | Ast.Ge -> Sge (rd, r1, r2)
          | Ast.Eq -> Seq (rd, r1, r2)
          | Ast.Ne -> Sne (rd, r1, r2)
          | _ -> failwith "Unsupported binary operator"
        in
        instrs1 @ instrs2 @ [op_instr]

    | UnaryOp (dest, op, operand) ->
        let rd = operand_to_reg dest in
        let (load_instrs, rs) = match operand with
          | Imm n -> let tmp = fresh_tmp () in ([Li (tmp, n)], tmp)
          | _ -> ([], operand_to_reg operand)
        in
        let op_instr = match op with
          | Ast.Neg -> Neg (rd, rs)
          | Ast.Not -> Seqz (rd, rs)
          | Ast.Pos -> Mv (rd, rs)
        in
        load_instrs @ [op_instr]

    | Jump lbl -> [J lbl]

    | BranchZero (op, lbl) ->
        (match op with
         | Imm n ->
             let tmp = fresh_tmp () in
             [Li (tmp, n); Beqz (tmp, lbl)]
         | _ ->
             let reg = operand_to_reg op in
             [Beqz (reg, lbl)])

    | BranchNonZero (op, lbl) ->
        (match op with
         | Imm n ->
             let tmp = fresh_tmp () in
             [Li (tmp, n); Bnez (tmp, lbl)]
         | _ ->
             let reg = operand_to_reg op in
             [Bnez (reg, lbl)])

    | Label lbl -> [Label lbl]
  in

   (* 为每个参数生成 sw 指令，把 a0‑a7 存入对应的栈槽 *)
  let save_params = List.mapi (fun i param ->
    let offset = get_var_offset param in
    Sw (PhysReg (Printf.sprintf "a%d" i), offset, PhysReg "fp")
  ) func.params in
  let prologue = [FrameSetup frame_aligned] in
  let body_instrs = List.concat_map select_instr func.body in
  let instrs = Label func.name :: prologue @ save_params @ body_instrs in
  { name = func.name;
    instrs = instrs;
    frame_size = frame_aligned }

  let select_program (prog : ir_program) : machine_program =
  let functions = List.map (fun f -> select_function f) prog.functions in
  { globals = prog.globals; functions }