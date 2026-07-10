open Ir

type block = {
  blk_label : label;
  blk_instrs : ir_instr list;
  blk_succ : label list;
}

module LabelMap = Map.Make(struct type t = label let compare = compare end)
type cfg = block LabelMap.t

(* 切分线性IR为基本块 *)
let split_blocks instrs =
  let rec aux cur_lbl cur_ins rest =
    match rest with
    | [] ->
        if cur_ins = [] then [] else [(cur_lbl, List.rev cur_ins)]
    | IrLabel l :: tail ->
        let rest_blk = aux l [] tail in
        if cur_ins = [] then rest_blk else (cur_lbl, List.rev cur_ins) :: rest_blk
    | instr :: tail ->
        let is_terminator = match instr with
          | IrJmp _ | IrCjmp _ | IrRet _ -> true
          | _ -> false
        in
        if is_terminator then
          let cur_full = (cur_lbl, List.rev (instr :: cur_ins)) in
          match tail with
          | IrLabel l :: t -> cur_full :: aux l [] t
          | _ -> cur_full :: aux (Label ".dummy") [] tail
        else
          aux cur_lbl (instr :: cur_ins) tail
  in
  aux (Label ".entry") [] instrs

(* 计算每个块的后继节点 *)
let build_block_succ (lbl, ins) : block =
  let last = List.hd (List.rev ins) in
  let succs = match last with
    | IrJmp (Label s) -> [Label s]
    | IrCjmp (_, Label t, Label f) -> [Label t; Label f]
    | IrRet _ -> []
    | _ -> []
  in
  { blk_label = lbl; blk_instrs = ins; blk_succ = succs }

(* 主函数：IR函数构建完整CFG *)
let build_cfg (f : ir_func) : cfg =
  let raw = split_blocks f.ir_body in
  let blocks = List.map build_block_succ raw in
  List.fold_left (fun m b -> LabelMap.add b.blk_label b m) LabelMap.empty blocks

(* 调试打印CFG结构 *)
let dump_cfg (g : cfg) =
  LabelMap.iter (fun (Label l) b ->
    Printf.printf "Block %s | succ: " l;
    List.iter (fun (Label s) -> Printf.printf "%s " s) b.blk_succ;
    print_newline ()
  ) g
