open Ir
open Riscv

let rec select_instr instr : riscv_instr list =
  match instr with
  | IrLabel (Label l) -> [RvLabel l]
  | IrIntLit (VReg d, n) -> [RvLi (VReg d, n)]
  | IrLoadVar (VReg d, x) -> [RvLw (VReg d, x, 0)]
  | IrStoreVar (x, VReg s) -> [RvSw (VReg s, x, 0)]
  | IrBinOp (VReg d, op, VReg l, VReg r) ->
      let rv_op = match op with
        | IrAdd -> RvAdd | IrSub -> RvSub | IrMul -> RvMul
        | IrDiv -> RvDiv | IrMod -> RvRem | IrLt -> RvSlt
        | IrLe -> RvSle | IrGt -> RvSgt | IrGe -> RvSge
        | IrEq -> RvSeqz | IrNe -> RvSnez | IrAnd -> RvAnd | IrOr -> RvOr
      in
      [RvBinOp (rv_op, VReg d, VReg l, VReg r)]
  | IrJmp (Label t) -> [RvJ t]
  | IrCjmp (VReg cond, Label t, Label f) ->
      [RvBne (VReg cond, VReg 0, t); RvJ f]
  | IrRet None -> [RvRet]
  | IrRet (Some (VReg r)) -> [RvMv (VReg 10, VReg r); RvRet]

let select_func f = List.concat (List.map select_instr f.ir_body)
