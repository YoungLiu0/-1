(** 到达定值分析 (Reaching Definitions) *)

open Dataflow

type definition = string * int

module DefLattice : LATTICE with type t = definition list = struct
  type t = definition list
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
  let to_string defs =
    let def_strs = List.map (fun (var, id) ->
      Printf.sprintf "%s@%d" var id) defs in
    "{" ^ String.concat ", " def_strs ^ "}"
end

(* 分析结果类型，与 dataflow 求解器的输出字段一致 *)
type result = {
  in_facts  : (string, DefLattice.t) Hashtbl.t;
  out_facts : (string, DefLattice.t) Hashtbl.t;
}

let analyze (cfg : Cfg.t) : result =
  let module T = struct
    type fact = DefLattice.t
    let instr_id = ref 0
    let get_def = function
      | Ir.Store (var, _) -> Some var
      | Ir.StoreGlobal (var, _) -> Some var
      | Ir.Alloc var -> Some var
      | _ -> None
    let transfer instr in_defs =
      incr instr_id;
      match get_def instr with
      | None -> in_defs
      | Some var ->
          let killed = List.filter (fun (v, _) -> v <> var) in_defs in
          (var, !instr_id) :: killed
  end in
  let module S = Solver(DefLattice)(T) in
  let res = S.analyze_forward cfg DefLattice.bottom in
  { in_facts = res.in_facts; out_facts = res.out_facts }