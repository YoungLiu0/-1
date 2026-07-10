(* lib/copy_prop.ml *)
open Ir
open Cfg


(** 拷贝关系：(dest_var, source_op) 列表 *)
module CopyLattice = struct
  type t = (string * operand) list

  let bottom () = []

  let equal c1 c2 =
    List.length c1 = List.length c2 &&
    List.for_all (fun pair -> List.mem pair c2) c1

  (** 取交集：只保留两个状态中都存在且一致的拷贝 *)
  let meet c1 c2 =
    List.filter (fun pair -> List.mem pair c2) c1
end

type copy_result = {
  in_facts : (string, CopyLattice.t) Hashtbl.t;
  out_facts : (string, CopyLattice.t) Hashtbl.t;
}

(** 转移函数 *)
let transfer (instr : ir_instr) (copies : CopyLattice.t) : CopyLattice.t =
  match instr with
  | Move (Temp t, (Temp _ | Local _ | Global _ as src)) ->
      let dest_var = "t" ^ string_of_int t in
      let copies' = List.filter (fun (v, _) -> v <> dest_var) copies in
      (dest_var, src) :: copies'
  
  | Move (Local v, (Temp _ | Local _ | Global _ as src)) ->
      let dest_var = "l_" ^ v in
      let copies' = List.filter (fun (v, _) -> v <> dest_var) copies in
      (dest_var, src) :: copies'
  
  | Move (Temp t, _) | BinOp (Temp t, _, _, _) | UnaryOp (Temp t, _, _)
  | Load (Temp t, _) | LoadGlobal (Temp t, _) | Call (Temp t, _, _) ->
      let dest_var = "t" ^ string_of_int t in
      List.filter (fun (v, _) -> v <> dest_var) copies
  
  | Move (Local v, _) | BinOp (Local v, _, _, _) | UnaryOp (Local v, _, _)
  | Load (Local v, _) | LoadGlobal (Local v, _) | Call (Local v, _, _)
  | Store (v, _) ->
      let dest_var = "l_" ^ v in
      List.filter (fun (v, _) -> v <> dest_var) copies
  
  | StoreGlobal (v, _) ->
      let dest_var = "g_" ^ v in
      List.filter (fun (v, _) -> v <> dest_var) copies
  
  | _ -> copies

(** 数据流分析 – 修正后的版本 *)
let analyze (cfg : Cfg.t):copy_result =
  let in_facts = Hashtbl.create (List.length cfg.labels) in
  let out_facts = Hashtbl.create (List.length cfg.labels) in
  
  List.iter (fun lbl ->
    Hashtbl.add in_facts lbl (CopyLattice.bottom ());
    Hashtbl.add out_facts lbl (CopyLattice.bottom ())
  ) cfg.labels;
  
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun lbl ->
      let preds = Cfg.get_predecessors cfg lbl in
      (* 修正：从 bottom() 开始，遍历所有前驱 *)
      let in_state = 
        List.fold_left (fun acc pred ->
          CopyLattice.meet acc (Hashtbl.find out_facts pred)
        ) (CopyLattice.bottom ()) preds
      in
      
      let block = Cfg.get_block cfg lbl in
      let out_state = List.fold_left (fun st instr ->
        transfer instr st
      ) in_state block.instrs in
      
      if not (CopyLattice.equal in_state (Hashtbl.find in_facts lbl)) ||
         not (CopyLattice.equal out_state (Hashtbl.find out_facts lbl)) then begin
        changed := true;
        Hashtbl.replace in_facts lbl in_state;
        Hashtbl.replace out_facts lbl out_state
      end
    ) cfg.labels
  done;
  
  {in_facts; out_facts }

(** 应用拷贝传播优化 *)
let apply_copy_prop (cfg : Cfg.t) (result :copy_result) : Cfg.t =
  let blocks' = Hashtbl.create (Hashtbl.length cfg.blocks) in
  
  List.iter (fun lbl ->
    let block = Cfg.get_block cfg lbl in
    let in_copies = Hashtbl.find result.in_facts lbl in
    
    let copies = ref in_copies in
    let optimized_instrs = List.map (fun instr ->
      let replace_op = function
        | Temp t as op ->
            let var = "t" ^ string_of_int t in
            (match List.assoc_opt var !copies with
             | Some replacement -> replacement
             | None -> op)
        | Local v as op ->
            let var = "l_" ^ v in
            (match List.assoc_opt var !copies with
             | Some replacement -> replacement
             | None -> op)
        | op -> op
      in
      
      let new_instr = match instr with
        | Move (dest, op) -> Move (dest, replace_op op)
        | BinOp (dest, op, op1, op2) ->
            BinOp (dest, op, replace_op op1, replace_op op2)
        | UnaryOp (dest, op, operand) ->
            UnaryOp (dest, op, replace_op operand)
        | BranchZero (op, lbl) -> BranchZero (replace_op op, lbl)
        | BranchNonZero (op, lbl) -> BranchNonZero (replace_op op, lbl)
        | Ret (Some op) -> Ret (Some (replace_op op))
        | Store (var, op) -> Store (var, replace_op op)
        | StoreGlobal (var, op) -> StoreGlobal (var, replace_op op)
        | other -> other
      in
      
      copies := transfer new_instr !copies;
      new_instr
    ) block.instrs in
    
    Hashtbl.add blocks' lbl { block with instrs = optimized_instrs }
  ) cfg.labels;
  
  { cfg with blocks = blocks' }

let global_copy_propagation (cfg : Cfg.t) : Cfg.t =
  let result = analyze cfg in
  apply_copy_prop cfg result