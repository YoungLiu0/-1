(** 活跃变量分析 (Live Variable Analysis) *)

open Dataflow

module LiveLattice : LATTICE with type t = string list = struct
  type t = string list
  let bottom = []
  let equal s1 s2 =
    let sorted1 = List.sort compare s1 in
    let sorted2 = List.sort compare s2 in
    sorted1 = sorted2
  let meet s1 s2 =
    let rec union acc = function
      | [] -> acc
      | x :: xs ->
          if List.mem x acc then union acc xs
          else union (x :: acc) xs
    in
    union s1 s2
  let to_string vars =
    "{" ^ String.concat ", " vars ^ "}"
end

type result = {
  in_facts  : (string, LiveLattice.t) Hashtbl.t;
  out_facts : (string, LiveLattice.t) Hashtbl.t;
}

let operand_to_string = function
  | Ir.Local v -> "l_" ^ v
  | Ir.Param v -> "p_" ^ v
  | Ir.Global v -> "g_" ^ v
  | Ir.Temp t -> "t" ^ string_of_int t
  | Ir.Imm _ -> ""

let get_uses instr =
  let extract op = match operand_to_string op with "" -> [] | s -> [s] in
  match instr with
  | Ir.Store (_, op) | Ir.StoreGlobal (_, op) -> extract op
  | Ir.Move (_, op) -> extract op
  | Ir.BinOp (_, _, op1, op2) -> extract op1 @ extract op2
  | Ir.UnaryOp (_, _, op) -> extract op
  | Ir.Call (_, _, args) -> List.concat_map extract args
  | Ir.BranchZero (op, _) | Ir.BranchNonZero (op, _) -> extract op
  | Ir.Ret (Some op) -> extract op
  | _ -> []

let get_def instr =
  let extract op = match operand_to_string op with "" -> None | s -> Some s in
  match instr with
  | Ir.Store (var, _) -> Some ("l_" ^ var)
  | Ir.StoreGlobal (var, _) -> Some ("g_" ^ var)
  | Ir.Alloc var -> Some ("l_" ^ var)
  | Ir.Move (dest, _) -> extract dest
  | Ir.BinOp (dest, _, _, _) -> extract dest
  | Ir.UnaryOp (dest, _, _) -> extract dest
  | Ir.Call (dest, _, _) -> extract dest
  | Ir.Load (dest, _) -> extract dest
  | Ir.LoadGlobal (dest, _) -> extract dest
  | _ -> None

let analyze (cfg : Cfg.t) : result =
  let module T = struct
    type fact = LiveLattice.t
    let transfer instr out_live =
      let uses = get_uses instr in
      let def = get_def instr in
      let after_kill = match def with
        | None -> out_live
        | Some var -> List.filter (fun v -> v <> var) out_live
      in
      LiveLattice.meet uses after_kill
  end in
  let module S = Solver(LiveLattice)(T) in
  let res = S.analyze_backward cfg LiveLattice.bottom in
  { in_facts = res.in_facts; out_facts = res.out_facts }
  (* 暴露给优化器使用的辅助函数 *)
let get_uses = get_uses
let get_def = get_def