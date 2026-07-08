(* 中端优化,在 CFG 上跑常量传播、死代码删除*)
(** IR 优化器（Step 1 直接返回原 CFG，不做任何优化） *)

open Cfg_builder

let optimize (cfg : cfg) : cfg =
  cfg