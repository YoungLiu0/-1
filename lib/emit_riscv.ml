open Riscv

let string_of_vreg (VReg n) = Printf.sprintf "x%d" n

let string_of_riscv instr =
  match instr with
  | RvLabel (Label l) -> l ^ ":"
  | RvLi (d, n) -> Printf.sprintf "li %s, %d" (string_of_vreg d) n
  | RvLw (d, s, off) -> Printf.sprintf "lw %s, %d(%s)" (string_of_vreg d) off s
  | RvSw (s, d, off) -> Printf.sprintf "sw %s, %d(%s)" (string_of_vreg s) off d
  | RvAdd (d, l, r) -> Printf.sprintf "add %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSub (d, l, r) -> Printf.sprintf "sub %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvMul (d, l, r) -> Printf.sprintf "mul %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvDiv (d, l, r) -> Printf.sprintf "div %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvRem (d, l, r) -> Printf.sprintf "rem %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSlt (d, l, r) -> Printf.sprintf "slt %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSle (d, l, r) -> Printf.sprintf "sle %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSgt (d, l, r) -> Printf.sprintf "sgt %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSge (d, l, r) -> Printf.sprintf "sge %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvSeqz (d, l, _) -> Printf.sprintf "seqz %s, %s" (string_of_vreg d) (string_of_vreg l)
  | RvSnez (d, l, _) -> Printf.sprintf "snez %s, %s" (string_of_vreg d) (string_of_vreg l)
  | RvAnd (d, l, r) -> Printf.sprintf "and %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvOr (d, l, r) -> Printf.sprintf "or %s, %s, %s" (string_of_vreg d) (string_of_vreg l) (string_of_vreg r)
  | RvJ (Label l) -> Printf.sprintf "j %s" l
  | RvBne (rs, rt, Label l) -> Printf.sprintf "bne %s, %s, %s" (string_of_vreg rs) (string_of_vreg rt) l
  | RvMv (d, s) -> Printf.sprintf "mv %s, %s" (string_of_vreg d) (string_of_vreg s)
  | RvRet -> "ret"

(* 输出完整函数汇编文本 *)
let emit_function name instrs =
  let header = Printf.sprintf ".global %s\n%s:\n" name name in
  let lines = List.map (fun i -> "  " ^ string_of_riscv i) instrs in
  header ^ String.concat "\n" lines
