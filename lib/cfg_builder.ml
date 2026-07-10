(* lib/cfg_builder.ml *)
open Ir
open Cfg

(* 内部使用的标签生成器 *)
let fresh_label =
  let counter = ref 0 in
  fun () ->
    let lbl = Printf.sprintf ".L%d" !counter in
    incr counter;
    lbl

(** 从 ir_func 构建 CFG *)
let build_cfg (func : ir_func) : Cfg.t =
  let blocks = Hashtbl.create 16 in
  let labels = ref [] in
  let current_label = ref "" in
  let current_instrs = ref [] in

  let emit_block term =
    if !current_label <> "" then begin
      let blk = {
        label  = !current_label;
        instrs = List.rev !current_instrs;
        terminator = term
      } in
      Hashtbl.add blocks !current_label blk;
      labels := !current_label :: !labels
    end
  in

  let rec process = function
    | Label lbl :: rest ->
        (* 遇到标签：结束当前块，开始新块 *)
        emit_block Unreachable;
        current_label := lbl;
        current_instrs := [];
        process rest

    | Jump lbl :: rest ->
        (* 遇到 Jump：不添加到 instrs，直接作为终止符 *)
        emit_block (Jump lbl);
        current_label := "";
        current_instrs := [];
        process rest

    | BranchZero (op, lbl) :: rest ->
        (* 遇到条件跳转：不添加到 instrs，直接作为终止符 *)
        emit_block (Branch (Zero, op, lbl, ""));
        current_label := "";
        current_instrs := [];
        process rest

    | BranchNonZero (op, lbl) :: rest ->
        (* 遇到条件跳转：不添加到 instrs，直接作为终止符 *)
        emit_block (Branch (NonZero, op, lbl, ""));
        current_label := "";
        current_instrs := [];
        process rest

    | (Ret _ as ret_instr) :: rest ->
      if !current_label = "" then current_label := fresh_label();
        (* 遇到 Ret：添加到 instrs（因为 Ret 指令本身携带返回值信息） *)
        current_instrs := ret_instr :: !current_instrs;
        emit_block Unreachable;
        current_label := "";
        current_instrs := [];
        process rest
   
    | instr :: rest when !current_label = "" ->
        (* 没有当前块，创建新块 *)
        current_label := fresh_label ();
        current_instrs := [instr];
        process rest

    | instr :: rest ->
        (* 普通指令：添加到当前块 *)
        current_instrs := instr :: !current_instrs;
        process rest

     |[]-> if !current_label <> "" then begin
          emit_block Unreachable
        end
  in

  let () = process func.body in

  let labels = List.rev !labels in
  let entry = match labels with
    | [] -> "entry"
    | hd :: _ -> hd
  in

  (* 计算后继列表 *)
  let succs = Hashtbl.create (List.length labels) in
  Hashtbl.iter (fun lbl blk ->
    let succ_list = match blk.terminator with
      | Unreachable -> 
        let last_is_ret = match List.rev blk.instrs with
        |Ret _:: _->true
        |_->false
      in
      if last_is_ret then []
      else
        let rec find_next lst = match lst with
        |a::b::_ when a = lbl ->[b]
        |_::rest ->find_next rest
        |[]->[]
        in find_next labels
      | Jump target -> [target]
      | Branch (_, _op, true_lbl, false_lbl) ->
          let targets = [true_lbl] in
          let false_target =
            if false_lbl <> "" then Some false_lbl
            else
              (* 隐式 fall-through 到下一个块 *)
              let rec find_next lst = match lst with
                | a :: b :: _ when a = lbl -> Some b
                | _ :: rest -> find_next rest
                | [] -> None
              in
              find_next labels
          in
          (match false_target with Some f -> f :: targets | None -> targets)
    in
    Hashtbl.add succs lbl succ_list
  ) blocks;

  (* 计算前驱列表 *)
  let preds = Hashtbl.create (List.length labels) in
  List.iter (fun lbl -> Hashtbl.add preds lbl []) labels;
  Hashtbl.iter (fun lbl succ_list ->
    List.iter (fun succ ->
      let old = try Hashtbl.find preds succ with Not_found -> [] in
      Hashtbl.replace preds succ (lbl :: old)
    ) succ_list
  ) succs;

  { blocks; labels; entry; preds; succs }