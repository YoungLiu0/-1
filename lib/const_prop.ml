(* lib/const_prop.ml *)
open Ir
open Cfg

(** 常量格：Bottom（未知） | Const n（常量） | Top（非常量） *)
type const_value = Bottom | Const of int | Top


module ConstLattice = struct
  type t = (string, const_value) Hashtbl.t

  let bottom () = Hashtbl.create 16

  let top () = 
    let t = Hashtbl.create 16 in
    Hashtbl.add t "__top__" Top;
    t

  let is_top facts =
    Hashtbl.mem facts "__top__"

  let equal s1 s2 =
    if Hashtbl.length s1 <> Hashtbl.length s2 then false
    else
      Hashtbl.fold (fun var val1 acc ->
        acc && (
          match Hashtbl.find_opt s2 var with
          | Some val2 -> val1 = val2
          | None -> false
        )
      ) s1 true

  let meet s1 s2 =
    let result = Hashtbl.create 16 in
    Hashtbl.iter (fun var val1 ->
      let merged = match Hashtbl.find_opt s2 var with
        | None -> val1
        | Some val2 ->
            if val1 = val2 then val1
            else Top
      in
      Hashtbl.add result var merged
    ) s1;
    Hashtbl.iter (fun var val2 ->
      if not (Hashtbl.mem result var) then
        Hashtbl.add result var val2
    ) s2;
    result
end

type const_result ={
in_facts:(string ,ConstLattice.t) Hashtbl.t;
out_facts:(string, ConstLattice.t) Hashtbl.t;
}

(** 转移函数 *)
let transfer (instr : ir_instr) (state : ConstLattice.t) : ConstLattice.t =
  let result = Hashtbl.copy state in
  
  let get_const_value = function
    | Imm n -> Const n
    | Temp t -> 
        (try Hashtbl.find state ("t" ^ string_of_int t)
         with Not_found -> Bottom)
    | Local v -> 
        (try Hashtbl.find state ("l_" ^ v)
         with Not_found -> Bottom)
    | Global v -> 
        (try Hashtbl.find state ("g_" ^ v)
         with Not_found -> Bottom)
   | Param _ -> Bottom
  in

  let set_value var value =
    Hashtbl.replace result var value
  in

  (match instr with
   | Move (Temp t, op) ->
       set_value ("t" ^ string_of_int t) (get_const_value op)
   
   | Move (Local v, op) ->
       set_value ("l_" ^ v) (get_const_value op)
   
   | BinOp (Temp t, op, op1, op2) ->
       (match get_const_value op1, get_const_value op2 with
        | Const n1, Const n2 ->
            let res = match op with
              | Ast.Add -> Const (n1 + n2)
              | Ast.Sub -> Const (n1 - n2)
              | Ast.Mul -> Const (n1 * n2)
              | Ast.Div -> if n2 = 0 then Top else Const (n1 / n2)
              | Ast.Mod -> if n2 = 0 then Top else Const (n1 mod n2)
              | Ast.Lt -> Const (if n1 < n2 then 1 else 0)
              | Ast.Le -> Const (if n1 <= n2 then 1 else 0)
              | Ast.Gt -> Const (if n1 > n2 then 1 else 0)
              | Ast.Ge -> Const (if n1 >= n2 then 1 else 0)
              | Ast.Eq -> Const (if n1 = n2 then 1 else 0)
              | Ast.Ne -> Const (if n1 <> n2 then 1 else 0)
              | _ -> Top
            in
            set_value ("t" ^ string_of_int t) res
        | Top, _ | _, Top -> set_value ("t" ^ string_of_int t) Top
        | _ -> set_value ("t" ^ string_of_int t) Bottom)
   
   | BinOp (Local v, _, _, _) ->
       set_value ("l_" ^ v) Top
   
   | UnaryOp (Temp t, op, operand) ->
       (match get_const_value operand with
        | Const n ->
            let res = match op with
              | Ast.Neg -> Const (-n)
              | Ast.Not -> Const (if n = 0 then 1 else 0)
              | Ast.Pos -> Const n
            in
            set_value ("t" ^ string_of_int t) res
        | Top -> set_value ("t" ^ string_of_int t) Top
        | Bottom -> ())
   
   | Store (var, _) -> set_value ("l_" ^ var) Top
   | StoreGlobal (var, _) -> set_value ("g_" ^ var) Top
   | Load (Temp t, _) -> set_value ("t" ^ string_of_int t) Top
   | Load (Local v, _) -> set_value ("l_" ^ v) Top
   | LoadGlobal (Temp t, _) -> set_value ("t" ^ string_of_int t) Top
   | LoadGlobal (Local v, _) -> set_value ("l_" ^ v) Top
   | Call (Temp t, _, _) -> set_value ("t" ^ string_of_int t) Top
   | Call (Local v, _, _) -> set_value ("l_" ^ v) Top
   
   | _ -> ());
  
  result

(** 数据流分析 *)
let analyze (cfg : Cfg.t):const_result =
  let in_facts = Hashtbl.create (List.length cfg.labels) in
  let out_facts = Hashtbl.create (List.length cfg.labels) in
  
  List.iter (fun lbl ->
    Hashtbl.add in_facts lbl (ConstLattice.bottom ());
    Hashtbl.add out_facts lbl (ConstLattice.bottom ())
  ) cfg.labels;
  
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun lbl ->
      let preds = Cfg.get_predecessors cfg lbl in
      let in_state = 
        List.fold_left (fun acc pred ->
          ConstLattice.meet acc (Hashtbl.find out_facts pred)
        ) (ConstLattice.bottom ()) preds
      in
      
      let block = Cfg.get_block cfg lbl in
      let out_state = List.fold_left (fun st instr ->
        transfer instr st
      ) in_state block.instrs in
      
      if not (ConstLattice.equal in_state (Hashtbl.find in_facts lbl)) ||
         not (ConstLattice.equal out_state (Hashtbl.find out_facts lbl)) then begin
        changed := true;
        Hashtbl.replace in_facts lbl in_state;
        Hashtbl.replace out_facts lbl out_state
      end
    ) cfg.labels
  done;
  {in_facts; out_facts }

(** 应用常量传播优化 *)
let apply_const_prop (cfg : Cfg.t) (result : const_result) : Cfg.t =
  let blocks' = Hashtbl.create (Hashtbl.length cfg.blocks) in
  
  List.iter (fun lbl ->
    let block = Cfg.get_block cfg lbl in
    let in_state = Hashtbl.find result.in_facts lbl in
    
    let state = ref (Hashtbl.copy in_state) in
    let optimized_instrs = List.map (fun instr ->
      let get_const_op = function
        | Temp t as op ->
            (match Hashtbl.find_opt !state ("t" ^ string_of_int t) with
             | Some (Const n) -> Imm n
             | _ -> op)
        | Local v as op ->
            (match Hashtbl.find_opt !state ("l_" ^ v) with
             | Some (Const n) -> Imm n
             | _ -> op)
        | op -> op
      in
      
      let new_instr = match instr with
        | Move (dest, op) -> Move (dest, get_const_op op)
        | BinOp (dest, op, op1, op2) ->
            BinOp (dest, op, get_const_op op1, get_const_op op2)
        | UnaryOp (dest, op, operand) ->
            UnaryOp (dest, op, get_const_op operand)
        | BranchZero (op, lbl) -> BranchZero (get_const_op op, lbl)
        | BranchNonZero (op, lbl) -> BranchNonZero (get_const_op op, lbl)
        | Ret (Some op) -> Ret (Some (get_const_op op))
        | other -> other
      in
      
      state := transfer new_instr !state;
      new_instr
    ) block.instrs in
    
    Hashtbl.add blocks' lbl { block with instrs = optimized_instrs }
  ) cfg.labels;
  
  { cfg with blocks = blocks' }

let global_constant_propagation (cfg : Cfg.t) : Cfg.t =
  let result = analyze cfg in
  apply_const_prop cfg result