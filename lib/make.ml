module type LIMIT_TYPE = sig
val max_limit : int
end
module type LIMITED_SET = sig
type t 
val empty : t
val add : int -> t -> t
val elements : t -> int list
end

module Make (M:LIMIT_TYPE):LIMITED_SET=struct
 type  t = int list;;
  let empty = [];;
  let add v ls=
  if List.length ls + 1 > M.max_limit then
    ls 
  else if List.mem v ls then
    ls 
  else
  List.sort (fun x y -> x - y) (v::ls) ;;
  let elements tl= tl;; 
end