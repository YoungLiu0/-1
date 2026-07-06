(** Interpreter for ToyLang *)

open Ast

type value = int

module StringMap = Map.Make (String)

(* Environment to store variable values *)
type value_env = value StringMap.t

let rec interpret_program (prog : program) : unit =
  ignore (exec_stmt_seq StringMap.empty prog)

and exec_stmt_seq (env : value_env) (stmts : stmt_seq) : value_env =
  List.fold_left exec_stmt env stmts

and exec_stmt (env : value_env) : stmt -> value_env = function
  | IfStmt (cond, then_body, else_body) ->
    let cond_val = eval_exp env cond in
    if cond_val <> 0
    then exec_stmt_seq env then_body
    else exec_stmt_seq env else_body
  | RepeatStmt (body, cond) -> let env' = exec_stmt_seq env body in
  let cond_val = eval_exp env' cond in
  if cond_val <> 0
    then env'
    else exec_stmt env' (RepeatStmt(body, cond))
  | AssignStmt (lval, rval) ->
    let rval_val = eval_exp env rval in
    StringMap.add lval rval_val env
  | PrintStmt e ->let v= eval_exp env e in
                  print_endline (string_of_int v);
                  env
and eval_exp (env : value_env) : exp -> value = function
  | IntExp n -> n
  | BoolExp b -> if b then 1 else 0
  | VarRefExp name ->try StringMap.find name env with
                    |Not_found ->failwith"Unbound name "^name  (* TODO: Handle name not found *)
  | BinaryExp (left, op, right) -> 
    let left_val = eval_exp env left in
    let right_val = eval_exp env right in
    match op with
    |AddOp->left_val + right_val
    |SubOp->left_val - right_val
    |MulOp -> left_val * right_val
    |DivOp -> if right_val = 0 then failwith "Division by zero"
           else left_val /right_val
    |LtOp -> if left_val < right_val then 1 else 0
    |EqOp -> if left_val =right_val then 1 else 0
;;
