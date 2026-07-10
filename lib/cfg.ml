(* lib/cfg.ml *)
open Ir

(** 条件跳转的类型 *)
type condition_kind = Zero | NonZero

(** 基本块的终止方式 *)
type terminator =
  | Unreachable
  | Jump of string
  | Branch of condition_kind * operand * string * string
  (* 删掉了 Return，因为 Ret 已经在 instrs 里 *)

(** 基本块 *)
type basic_block = {
  label      : string;
  instrs     : ir_instr list;
  terminator : terminator;
}

(** 控制流图 *)
type t = {
  blocks : (string, basic_block) Hashtbl.t;
  labels : string list;
  entry  : string;
  preds  : (string, string list) Hashtbl.t;
  succs  : (string, string list) Hashtbl.t;
}

(** 查询函数 *)
let get_successors (cfg : t) (label : string) : string list =
  try Hashtbl.find cfg.succs label with Not_found -> []

let get_predecessors (cfg : t) (label : string) : string list =
  try Hashtbl.find cfg.preds label with Not_found -> []

let get_block (cfg : t) (label : string) : basic_block =
  Hashtbl.find cfg.blocks label

(** 将 CFG 转回线性 IR 指令序列 *)
let cfg_to_linear (cfg : t) : ir_instr list =
  List.concat_map (fun lbl ->
    let blk = Hashtbl.find cfg.blocks lbl in
    let term_instrs : ir_instr list = match blk.terminator with
      | Unreachable -> []
      | Jump target -> [Ir.Jump target]
      | Branch (cond_kind, op, true_lbl, _) ->
          (match cond_kind with
           | Zero -> [Ir.BranchZero (op, true_lbl)]
           | NonZero -> [Ir.BranchNonZero (op, true_lbl)])
    in
    (Ir.Label lbl) :: blk.instrs @ term_instrs
  ) cfg.labels