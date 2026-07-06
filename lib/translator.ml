(** Three-Address Code Generator with short-circuit and scope renaming *)

open Ast

(* ---- Three-Address Code Instructions ---- *)
type tac_inst =
  | AssignLit of string * int
  | AssignBin of string * string * string * string
  | AssignVar of string * string
  | IfGoto of string * string
  | IfRelGoto of string * string * string * string
  | Goto of string
  | Print of string
  | Label of string

(* ---- Fresh name generators,生成全新的寄存器 ---- *)
let fresh_temp =
  let counter = ref 0 in
  fun () -> incr counter; "t" ^ string_of_int !counter
(*generate new label*)
let fresh_label =
  let counter = ref 0 in
  fun () -> incr counter; "L" ^ string_of_int !counter

(* ---- Scope and environment ---- *)
type env = (string * string) list   (* source name -> unique internal name *)

(*作用域id*)
  let current_scope_id = ref 0

(*分配新的作用域id*)
let enter_scope () =
  incr current_scope_id;
  !current_scope_id

(*找变量对应的唯一标识符*)
let lookup_var (env : env) (name : string) : string =
  match List.assoc_opt name env with
  | Some internal -> internal
  | None -> failwith ("Unbound name " ^ name)

  (*分配一个标识符，如果变量 name 在环境（由内向外）中已经存在 → 说明它在外层或当前作用域已定义，直接返回已有的内部名（重新赋值的情况）。
如果不存在 → 说明是当前作用域的新定义，用当前 scope_id 生成新的内部名（如 name_1）并加入环境头部。*)
let def_var (env : env) (name : string) (scope_id : int) : env * string =
  match List.assoc_opt name env with
  | Some existing -> (env, existing)   (* already defined in some scope, reuse internal name *)
  | None ->
      let internal = name ^ "_" ^ string_of_int scope_id in
      ((name, internal) :: env, internal)

(* ---- Helper functions ---- *)
let string_of_binop (op : Ast.binary_op) : string =
  match op with
  | Ast.AddOp -> "+" | Ast.SubOp -> "-" | Ast.MulOp -> "*" | Ast.DivOp -> "/"
  | Ast.LtOp  -> "<" | Ast.EqOp  -> "=="

(* ---- Value translation: returns (internal name, instructions) ---- *)
let rec translate_exp (env : env) (e : Ast.exp) : string * tac_inst list =
  match e with
  | Ast.IntExp n ->
      let t = fresh_temp () in
      (t, [AssignLit (t, n)])
  | Ast.BoolExp b ->
      let t = fresh_temp () in
      (t, [AssignLit (t, if b then 1 else 0)])
  | Ast.VarRefExp name ->
      let internal = lookup_var env name in
      (internal, [])
  | Ast.BinaryExp (left, op, right) ->
      let (l, instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
      let t = fresh_temp () in
      let op_str = string_of_binop op in
      (t, instrs1 @ instrs2 @ [AssignBin (t, l, op_str, r)])
  | Ast.AndExp (left, right) ->
      let  (l, instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
      let t = fresh_temp () in
       let op_str = "&&" in
        (t, instrs1 @ instrs2 @ [AssignBin (t, l, op_str, r)])
  | Ast.OrExp (left, right) ->
      let  (l, instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
      let t = fresh_temp () in
       let op_str = "||" in
        (t, instrs1 @ instrs2 @ [AssignBin (t, l, op_str, r)])

(* ---- Conditional translation with tentry/fentry (short-circuit) ---- *)
and translate_cond (env : env) (e : Ast.exp) (tentry : string) (fentry : string) : tac_inst list =
  match e with
  | Ast.BoolExp true  -> [Goto tentry]
  | Ast.BoolExp false -> [Goto fentry]
  | Ast.VarRefExp name ->
      let internal = lookup_var env name in
      [IfGoto (internal, tentry); Goto fentry]
  | Ast.BinaryExp (left, op, right) when op = Ast.LtOp || op = Ast.EqOp ->
      let (l, instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
      let relop = if op = Ast.LtOp then "<" else "==" in
      instrs1 @ instrs2 @ [IfRelGoto (l, relop, r, tentry); Goto fentry]
  | Ast.AndExp (left, right) ->
      let (l,instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
  instrs1@[IfGoto (not l, fentry)]@instrs2@[IfGoto (r, tentry); Goto fentry]
  | Ast.OrExp (left, right) ->
      let (l,instrs1) = translate_exp env left in
      let (r, instrs2) = translate_exp env right in
  instrs1@[IfGoto (l, tentry)]@instrs2@[IfGoto (r, tentry); Goto fentry]
  | _ ->   (* fallback: compute value, then branch *)
      let (v, instrs) = translate_exp env e in
      instrs @ [IfGoto (v, tentry); Goto fentry]

(* ---- Statement translation ---- *)
let rec translate_stmt (env : env) (scope_id : int) (stmt : Ast.stmt) : env * tac_inst list =
  match stmt with
  | Ast.AssignStmt (lval, rval) ->
      let (src, instrs) = translate_exp env rval in
      let (new_env, internal) = def_var env lval scope_id in
      (new_env, instrs @ [AssignVar (internal, src)])
  | Ast.PrintStmt e ->
      let (src, instrs) = translate_exp env e in
      (env, instrs @ [Print src])
  | Ast.IfStmt (cond, then_body, else_body) ->
    let tentry = fresh_label() in
    let fentry = fresh_label() in
    let exit = fresh_label ()in
    let instrs=begin
      if else_body = [] then
      translate_cond env cond tentry exit
       else 
       translate_cond env cond tentry fentry 
       end
      in
     let tid = enter_scope ()in
     let fid = enter_scope () in
     let (env1,instr1) = translate_stmt_seq env then_body tid in
     let instrs1 = instr1@[Goto exit] in
     let (env2,instrs2) = translate_stmt_seq env else_body fid in
     if not (else_body = []) then
      (env,instrs@[Label tentry]@instrs1@[Label fentry]@instrs2@[Label exit])
     else
      (env,instrs@[Label tentry]@instrs1@[Label exit])
  | Ast.RepeatStmt (body, cond) ->
    let id = enter_scope ()in
      let (env',instrs1) = translate_stmt_seq env body id in
      let tentry = fresh_label() in
      let fentry = fresh_label() in
      let instrs2 = translate_cond env' cond tentry fentry in
      (env,[Label fentry]@instrs1@instrs2@[Label tentry]) 
and translate_stmt_seq (env : env) (stmts : Ast.stmt list) (scope_id : int) : env * tac_inst list =
  List.fold_left (fun (env_acc, instrs_acc) stmt ->
    let (env_next, instrs) = translate_stmt env_acc scope_id stmt in
    (env_next, instrs_acc @ instrs)
  ) (env, []) stmts

let translate_program (prog : Ast.program) : tac_inst list =
  let init_scope = !current_scope_id in
  let (_, instrs) = translate_stmt_seq [] prog init_scope in
  instrs

(* ---- Printing ---- *)
let string_of_tac_inst (inst : tac_inst) : string =
  match inst with
  | AssignLit (dest, n) -> dest ^ " := " ^ string_of_int n
  | AssignBin (dest, left, op, right) ->
      dest ^ " := " ^ left ^ " " ^ op ^ " " ^ right
  | AssignVar (dest, src) -> dest ^ " := " ^ src
  | IfGoto (cond, label) -> "if " ^ cond ^ " goto " ^ label
  | IfRelGoto (left, relop, right, label) ->
      "if " ^ left ^ " " ^ relop ^ " " ^ right ^ " goto " ^ label
  | Goto label -> "goto " ^ label
  | Print op -> "print " ^ op
  | Label label -> label ^ ":"

let print_tac (instrs : tac_inst list) : unit =
  List.iter (fun i -> print_endline (string_of_tac_inst i)) instrs

(* Entry point called by the driver *)
let run (prog : Ast.program) : unit =
  let instrs = translate_program prog in
  print_tac instrs