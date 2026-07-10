(* 通用数据流分析框架 *)
open Cfg
(* 数据流分析方向 *)
type direction = Forward | Backward

(** 格（Lattice）的抽象接口 *)
module type LATTICE = sig
  type t
  val bottom : t
  val equal : t -> t -> bool
  val meet : t -> t -> t
  val to_string : t -> string
end

(** 转移函数接口 *)
module type TRANSFER = sig
  type fact
  val transfer : Ir.ir_instr -> fact -> fact
end

(** 数据流分析求解器 *)
module Solver (L : LATTICE) (T : TRANSFER with type fact = L.t) = struct
  
  type analysis_result = {
    in_facts  : (string, L.t) Hashtbl.t;
    out_facts : (string, L.t) Hashtbl.t;
  }

  (** 对单个基本块应用转移函数 *)
  let apply_transfer_block (instrs : Ir.ir_instr list) (in_fact : L.t) : L.t =
    List.fold_left (fun fact instr -> T.transfer instr fact) in_fact instrs

  (** 前向数据流分析 *)
  let analyze_forward (cfg : Cfg.t) (init : L.t) : analysis_result =
    let in_facts = Hashtbl.create 64 in
    let out_facts = Hashtbl.create 64 in
    
    List.iter (fun lbl ->
      if lbl = cfg.entry then
        Hashtbl.add in_facts lbl init
      else
        Hashtbl.add in_facts lbl L.bottom;
      Hashtbl.add out_facts lbl L.bottom
    ) cfg.labels;
    
    let changed = ref true in
    let max_iterations = 1000 in
    let iter_count = ref 0 in
    
    while !changed && !iter_count < max_iterations do
      changed := false;
      incr iter_count;
      
      List.iter (fun lbl ->
        let block = Cfg.get_block cfg lbl in
        let preds = Cfg.get_predecessors cfg lbl in
        
        let in_fact = 
          if lbl = cfg.entry then
            init
          else if List.length preds = 0 then
            L.bottom
          else
            List.fold_left (fun acc pred_lbl ->
              let pred_out = Hashtbl.find out_facts pred_lbl in
              L.meet acc pred_out
            ) L.bottom preds
        in
        
        let out_fact = apply_transfer_block block.instrs in_fact in
        
        let old_in = Hashtbl.find in_facts lbl in
        let old_out = Hashtbl.find out_facts lbl in
        
        if not (L.equal in_fact old_in) || not (L.equal out_fact old_out) then (
          changed := true;
          Hashtbl.replace in_facts lbl in_fact;
          Hashtbl.replace out_facts lbl out_fact
        )
      ) cfg.labels
    done;
    
    { in_facts; out_facts }

  (** 后向数据流分析 *)
  let analyze_backward (cfg : Cfg.t) (init : L.t) : analysis_result =
    let in_facts = Hashtbl.create 64 in
    let out_facts = Hashtbl.create 64 in
    
    List.iter (fun lbl ->
      Hashtbl.add in_facts lbl L.bottom;
      let succs = Cfg.get_successors cfg lbl in
      if List.length succs = 0 then
        Hashtbl.add out_facts lbl init
      else
        Hashtbl.add out_facts lbl L.bottom
    ) cfg.labels;
    
    let changed = ref true in
    let max_iterations = 1000 in
    let iter_count = ref 0 in
    
    while !changed && !iter_count < max_iterations do
      changed := false;
      incr iter_count;
      
      List.iter (fun lbl ->
        let block = Cfg.get_block cfg lbl in
        let succs = Cfg.get_successors cfg lbl in
        
        let out_fact =
          if List.length succs = 0 then
            init
          else
            List.fold_left (fun acc succ_lbl ->
              let succ_in = Hashtbl.find in_facts succ_lbl in
              L.meet acc succ_in
            ) L.bottom succs
        in
        
        let in_fact = apply_transfer_block (List.rev block.instrs) out_fact in
        
        let old_in = Hashtbl.find in_facts lbl in
        let old_out = Hashtbl.find out_facts lbl in
        
        if not (L.equal in_fact old_in) || not (L.equal out_fact old_out) then (
          changed := true;
          Hashtbl.replace in_facts lbl in_fact;
          Hashtbl.replace out_facts lbl out_fact
        )
      ) cfg.labels
    done;
    
    { in_facts; out_facts }
end