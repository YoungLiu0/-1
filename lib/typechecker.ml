open Ast

type typ =
  | IntType
  | BoolType

module StringMap = Map.Make (String)
type type_env = typ StringMap.t

let rec check_program (prog : program) : unit =
  List.iter (fun def ->
    match def with
    | FuncDef f -> ignore (check_func StringMap.empty f 0)
    | _ -> ()
  ) prog

and check_func (env : type_env) (f : func_def) (loop_depth : int) : type_env =
  let env_params = List.fold_left (fun e p -> StringMap.add p IntType e) env f.f_params in
  ignore (check_stmt env_params f.f_body loop_depth);
  env_params

and check_stmt (env : type_env) (s : stmt) (loop_depth : int) : type_env =
  match s with
  | Block stmts ->
      List.fold_left (fun e sub -> check_stmt e sub loop_depth) env stmts
  | EmptyStmt -> env
  | ExprStmt e -> ignore (infer_exp_type env e); env
  | Assign (x, e) ->
      let t = infer_exp_type env e in
      (match StringMap.find_opt x env with
       | Some old when old <> t -> failwith ("变量" ^ x ^ "赋值类型不匹配")
       | Some _ -> env
       | None -> StringMap.add x t env)
  | VarDecl (x, e) | ConstDecl (x, e) ->
      let t = infer_exp_type env e in
      StringMap.add x t env
  | If (cond, tstmt, estmt) ->
      let ct = infer_exp_type env cond in
      if ct <> BoolType then failwith "if 判断条件必须为布尔表达式";
      ignore (check_stmt env tstmt loop_depth);
      Option.iter (fun s -> ignore (check_stmt env s loop_depth)) estmt;
      env
  | While (cond, body) ->
      let ct = infer_exp_type env cond in
      if ct <> BoolType then failwith "while 判断条件必须为布尔表达式";
      ignore (check_stmt env body (loop_depth + 1));
      env
  | Break ->
      if loop_depth = 0 then failwith "break 只能出现在循环内部"; env
  | Continue ->
      if loop_depth = 0 then failwith "continue 只能出现在循环内部"; env
  | Return None -> env
  | Return (Some e) ->
      ignore (infer_exp_type env e); env

and infer_exp_type (env : type_env) (e : expr) : typ =
  match e with
  | IntLit _ -> IntType
  | Var x ->
      (try StringMap.find x env with Not_found -> failwith ("未定义变量：" ^ x))
  | Unary (op, e) ->
      let t = infer_exp_type env e in
      (match op with
       | Pos | Neg ->
           if t = IntType then IntType else failwith "正负运算符仅支持整数"
       | Not ->
           if t = BoolType then BoolType else failwith "! 仅支持布尔表达式")
  | Binary (op, e1, e2) ->
      let t1 = infer_exp_type env e1 in
      let t2 = infer_exp_type env e2 in
      (match op with
       | Add | Sub | Mul | Div | Mod ->
           if t1 = IntType && t2 = IntType then IntType
           else failwith "算术运算符操作数必须为int"
       | Lt | Le | Gt | Ge | Eq | Ne ->
           if t1 = t2 then BoolType else failwith "比较运算符两侧类型必须一致"
       | And | Or ->
           if t1 = BoolType && t2 = BoolType then BoolType
           else failwith "&& || 两侧必须是布尔表达式")
  | Call (_, args) ->
      List.iter (fun e -> ignore (infer_exp_type env e)) args;
      IntType
