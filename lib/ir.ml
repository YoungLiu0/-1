open Ast

(* IR 基础类型 *)
type label = Label of string
type vreg = VReg of int

type ir_binop =
  | IrAdd | IrSub | IrMul | IrDiv | IrMod
  | IrLt | IrLe | IrGt | IrGe | IrEq | IrNe
  | IrAnd | IrOr

type ir_instr =
  | IrLabel of label
  | IrBinOp of vreg * ir_binop * vreg * vreg
  | IrLoadVar of vreg * string
  | IrStoreVar of string * vreg
  | IrIntLit of vreg * int
  | IrJmp of label
  | IrCjmp of vreg * label * label
  | IrRet of vreg option

type ir_func = {
  ir_name : string;
  ir_params : string list;
  ir_body : ir_instr list;
}

(* 全局唯一标签/虚拟寄存器生成器 *)
let lbl_cnt = ref 0
let vreg_cnt = ref 0

let new_label () : label =
  let n = !lbl_cnt in incr lbl_cnt; Label (".L" ^ string_of_int n)
let new_vreg () : vreg =
  let n = !vreg_cnt in incr vreg_cnt; VReg n

(* 翻译表达式：返回 (结果虚拟寄存器, IR指令列表) *)
let rec translate_expr (e : expr) : vreg * ir_instr list =
  match e with
  | IntLit n ->
      let vr = new_vreg () in vr, [IrIntLit (vr, n)]
  | Var x ->
      let vr = new_vreg () in vr, [IrLoadVar (vr, x)]
  | Unary (op, e) ->
      let vr_e, ir_e = translate_expr e in
      let vr_dst = new_vreg () in
      (match op with
       | Pos -> vr_dst, ir_e @ [IrBinOp (vr_dst, IrAdd, vr_e, VReg 0)]
       | Neg -> vr_dst, ir_e @ [IrBinOp (vr_dst, IrSub, VReg 0, vr_e)]
       | Not ->
           let vr_zero = new_vreg () in
           let ir_zero = [IrIntLit (vr_zero, 0)] in
           vr_dst, ir_e @ ir_zero @ [IrBinOp (vr_dst, IrEq, vr_e, vr_zero)])
  | Binary (op, e1, e2) ->
      (* 短路 && 逻辑与 *)
      if op = And then
        let vr1, ir1 = translate_expr e1 in
        let l_false = new_label () in
        let l_end = new_label () in
        let vr_res = new_vreg () in
        let ir_cjmp = IrCjmp (vr1, l_end, l_false) in
        let vr2, ir2 = translate_expr e2 in
        let ir_one = IrIntLit (vr_res, 1) in
        let ir_zero = IrIntLit (vr_res, 0) in
        vr_res, ir1 @ [ir_cjmp]
        @ ir2 @ [IrBinOp (vr_res, IrOr, vr2, VReg 0); IrJmp l_end]
        @ [IrLabel l_false; ir_zero; IrLabel l_end]
      (* 短路 || 逻辑或 *)
      else if op = Or then
        let vr1, ir1 = translate_expr e1 in
        let l_true = new_label () in
        let l_end = new_label () in
        let vr_res = new_vreg () in
        let ir_cjmp = IrCjmp (vr1, l_true, l_end) in
        let vr2, ir2 = translate_expr e2 in
        let ir_one = IrIntLit (vr_res, 1) in
        vr_res, ir1 @ [ir_cjmp; IrLabel l_true; ir_one; IrJmp l_end]
        @ ir2 @ [IrBinOp (vr_res, IrOr, vr2, VReg 0); IrLabel l_end]
      (* 普通二元运算 *)
      else
        let vr1, ir1 = translate_expr e1 in
        let vr2, ir2 = translate_expr e2 in
        let vr_dst = new_vreg () in
        let ir_op = match op with
          | Add -> IrAdd | Sub -> IrSub | Mul -> IrMul | Div -> IrDiv | Mod -> IrMod
          | Lt -> IrLt | Le -> IrLe | Gt -> IrGt | Ge -> IrGe | Eq -> IrEq | Ne -> IrNe
          | _ -> failwith "unreachable"
        in
        vr_dst, ir1 @ ir2 @ [IrBinOp (vr_dst, ir_op, vr1, vr2)]
  | Call (_, args) ->
      let vr = new_vreg () in vr, []

(* 翻译语句，loop_stack：存储每层循环(头部标签, 出口标签)，支持多层嵌套break/continue *)
let rec translate_stmt (loop_stack : (label * label) list) (s : stmt) : ir_instr list =
  match s with
  | Block stmts -> List.concat (List.map (translate_stmt loop_stack) stmts)
  | EmptyStmt -> []
  | ExprStmt e -> snd (translate_expr e)
  | Assign (x, e) ->
      let vr, ir_e = translate_expr e in ir_e @ [IrStoreVar (x, vr)]
  | VarDecl (x, e) | ConstDecl (x, e) ->
      let vr, ir_e = translate_expr e in ir_e @ [IrStoreVar (x, vr)]
  | If (cond, tstmt, estmt_opt) ->
      let vr_cond, ir_cond = translate_expr cond in
      let l_else = new_label () in
      let l_end = new_label () in
      let cjmp = IrCjmp (vr_cond, l_end, l_else) in
      let then_ir = translate_stmt loop_stack tstmt in
      let jmp_end = IrJmp l_end in
      let else_ir = match estmt_opt with
        | Some s -> IrLabel l_else :: translate_stmt loop_stack s
        | None -> [IrLabel l_else]
      in
      ir_cond @ [cjmp] @ then_ir @ [jmp_end] @ else_ir @ [IrLabel l_end]
  | While (cond, body) ->
      let l_head = new_label () in
      let l_exit = new_label () in
      let new_stack = (l_head, l_exit) :: loop_stack in
      let label_h = IrLabel l_head in
      let vr_c, ir_c = translate_expr cond in
      let cjmp = IrCjmp (vr_c, l_head, l_exit) in
      let body_ir = translate_stmt new_stack body in
      let jmp_h = IrJmp l_head in
      let label_e = IrLabel l_exit in
      [label_h] @ ir_c @ [cjmp] @ body_ir @ [jmp_h; label_e]
  | Break ->
      (match loop_stack with (_, exit)::_ -> [IrJmp exit] | [] -> [])
  | Continue ->
      (match loop_stack with (head, _)::_ -> [IrJmp head] | [] -> [])
  | Return None -> [IrRet None]
  | Return (Some e) ->
      let vr, ir_e = translate_expr e in ir_e @ [IrRet (Some vr)]

(* 顶层入口：AST程序转IR函数列表 *)
let translate_program (prog : program) : ir_func list =
  List.map (fun def ->
    match def with
    | FuncDef f ->
        lbl_cnt := 0; vreg_cnt := 0;
        let body = translate_stmt [] f.f_body in
        { ir_name = f.f_name; ir_params = f.f_params; ir_body = body }
    | _ -> failwith "暂不处理全局变量"
  ) prog
