(* lib/loopInfo.ml *)

type loop_info = {
  header  : string;   (* while_start 标签 *)
  body    : string;   (* while_body 标签 *)
  exit    : string;   (* while_end 标签 *)
  latch   : string;   (* 回跳块标签，通常是 body *)
  cont_lbl: string;   (* continue 目标，即 header *)
  break_lbl: string;  (* break 目标，即 exit *)
  has_break_continue : bool ref;  (* 循环体内是否有 break/continue *)
}

(* 全局循环信息表，以 header 标签为键 *)
let loop_table : (string, loop_info) Hashtbl.t = Hashtbl.create 16

let register_loop header body exit latch cont_lbl break_lbl =
  let info = { header; body; exit; latch; cont_lbl; break_lbl;
               has_break_continue = ref false } in
  Hashtbl.replace loop_table header info

let get_loop header = Hashtbl.find_opt loop_table header

let all_loops () =
  Hashtbl.fold (fun _ info acc -> info :: acc) loop_table []

(* 标记循环包含 break/continue *)
let mark_loop_has_break_continue header =
  match get_loop header with
  | Some info -> info.has_break_continue := true
  | None -> ()