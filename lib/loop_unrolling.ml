(* lib/loop_unrolling.ml *)

open Ir

(* ========== 临时变量重命名 ========== *)
let temp_rename_map = Hashtbl.create 64

let fresh_temp_for old_t =
  match Hashtbl.find_opt temp_rename_map old_t with
  | Some new_op -> new_op
  | None ->
      let new_op = fresh_temp () in
      Hashtbl.add temp_rename_map old_t new_op;
      new_op

let rename_op = function
  | Temp t -> fresh_temp_for t
  | other  -> other

let rename_instr instr =
  match instr with
  | Move (dest, src) -> Move (rename_op dest, rename_op src)
  | BinOp (dest, op, op1, op2) ->
      BinOp (rename_op dest, op, rename_op op1, rename_op op2)
  | UnaryOp (dest, uop, op) ->
      UnaryOp (rename_op dest, uop, rename_op op)
  | Load (dest, var) -> Load (rename_op dest, var)
  | Store (var, src) -> Store (var, rename_op src)
  | Call (dest, fname, args) ->
      Call (rename_op dest, fname, List.map rename_op args)
  | LoadGlobal (dest, name) -> LoadGlobal (rename_op dest, name)
  | StoreGlobal (name, src) -> StoreGlobal (name, rename_op src)
  | BranchZero (cond, lab) -> BranchZero (rename_op cond, lab)
  | BranchNonZero (cond, lab) -> BranchNonZero (rename_op cond, lab)
  | Jump lab -> Jump lab
  | Label lab -> Label lab
  | instr -> instr   (* Ret, Alloc 等不会出现在循环体内 *)

(* ========== 在指令列表中定位标签索引 ========== *)
let find_label_index label instrs =
  let rec aux i = function
    | [] -> None
    | Label l :: _ when l = label -> Some i
    | _ :: rest -> aux (i+1) rest
  in
  aux 0 instrs

(* ========== 展开一个循环（指令列表面） ========== *)
let unroll_one_loop loop (instrs : ir_instr list) : ir_instr list =
  let header_lbl = loop.LoopInfo.header in
  let body_lbl   = loop.LoopInfo.body in
  let exit_lbl   = loop.LoopInfo.exit in

  match find_label_index header_lbl instrs with
  | None ->
      Printf.eprintf "[UNROLL] warning: header label %s not found, skipping\n" header_lbl;
      instrs
  | Some hdr_idx ->
  match find_label_index body_lbl instrs with
  | None ->
      Printf.eprintf "[UNROLL] warning: body label %s not found, skipping\n" body_lbl;
      instrs
  | Some body_idx ->
      Printf.eprintf "[UNROLL] labels found: hdr=%d body=%d\n" hdr_idx body_idx;
      let header_slice = List.filteri (fun i _ -> i >= hdr_idx && i < body_idx) instrs in
      match List.rev header_slice with
      | (BranchZero (cond, lab)) :: rest when lab = exit_lbl ->
          let cond_instrs = List.rev rest in
          let cond_instrs = List.filter (function Label _ -> false | _ -> true) cond_instrs in
          let exit_idx_opt = find_label_index exit_lbl instrs in
          let after_body = match exit_idx_opt with Some i -> i | None -> List.length instrs in
          let body_slice = List.filteri (fun i _ -> i >= body_idx && i < after_body) instrs in
          let (body_core, ends_correctly) =
            match List.rev body_slice with
            | (Jump lab) :: rest when lab = header_lbl -> (List.rev rest, true)
            | _ -> (body_slice, false)
          in
          let has_call = List.exists (function Call _ -> true | _ -> false) (cond_instrs @ body_core) in
          Printf.eprintf "[UNROLL] body ends_correctly=%b has_call=%b\n" ends_correctly has_call;
          if has_call then (Printf.eprintf "[UNROLL] skipped due to call\n"; instrs)
          else if not ends_correctly then (Printf.eprintf "[UNROLL] skipped: body structure unexpected\n"; instrs)
          else begin
            Printf.eprintf "[UNROLL] performing unroll...\n";
            Hashtbl.clear temp_rename_map;
            let new_cond_instrs = List.map rename_instr cond_instrs in
            let new_cond = rename_op cond in
            let new_body_lbl = fresh_label "unrolled_body" in
            let modified_body =
              body_core @ new_cond_instrs @
              [BranchZero (new_cond, exit_lbl); Jump new_body_lbl]
            in
            let new_body_core =
              List.filter (function Label _ -> false | _ -> true) body_core
              |> List.map rename_instr
            in
            let unrolled_block =
              Label new_body_lbl :: new_body_core @ [Jump header_lbl]
            in
            let prefix = List.filteri (fun i _ -> i < body_idx) instrs in
            let suffix = List.filteri (fun i _ -> i >= after_body) instrs in
            prefix @ modified_body @ unrolled_block @ suffix
          end
      | _ ->
          Printf.eprintf "[UNROLL] warning: unexpected header structure in %s, skipping\n" header_lbl;
          instrs (* ========== 主展开函数 ========== *)
let unroll_all_loops (instrs : ir_instr list) : ir_instr list =
  let loops = LoopInfo.all_loops () in
   Printf.eprintf "[UNROLL] total loops found: %d\n" (List.length loops);
  let instrs_ref = ref instrs in
  List.iter (fun loop ->
     Printf.eprintf "[UNROLL] loop %s: has_break_continue=%b\n"
      loop.LoopInfo.header !(loop.LoopInfo.has_break_continue);
    if !(loop.LoopInfo.has_break_continue) then ()
    else
      instrs_ref := unroll_one_loop loop !instrs_ref
  ) loops;
  !instrs_ref

(* ========== 对 CFG 进行循环展开 ========== *)
let unroll_loops_once (cfg : Cfg.t) : Cfg.t =
  let linear = Cfg.cfg_to_linear cfg in
  let new_linear = unroll_all_loops linear in
  (* 利用现有的 build_cfg 重建 CFG *)
  let dummy_func = {
    Ir.name = ""; ret_type = Ast.Int; params = []; body = new_linear; locals = []
  } in
  Cfg_builder.build_cfg dummy_func