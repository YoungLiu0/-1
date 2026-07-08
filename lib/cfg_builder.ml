(*将经过语义分析（类型检查）后的 AST（抽象语法树），翻译成带有明确控制流结构的三地址码（IR），并组织成基本块（Basic Block）列表（即 CFG）*)
(** 控制流图构建（Step 1 仅创建单基本块的简单实现） *)

open Ir

type basic_block = {
  label  : string;
  instrs : ir_instr list;
  mutable succs : string list;   (* 后继块标签 *)
}

type cfg = {
  blocks : (string, basic_block) Hashtbl.t;
  entry  : string;               (* 入口基本块标签 *)
}

let build_cfg (func : ir_func) : cfg =
  let entry_label = "entry" in
  let blocks = Hashtbl.create 2 in
  let entry_block = {
    label  = entry_label;
    instrs = func.body;
    succs  = [];
  } in
  Hashtbl.add blocks entry_label entry_block;
  { blocks; entry = entry_label }