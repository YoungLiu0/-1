
let adjacency edges n =
  let nodes =List.init n (fun i -> i + 1) in
  let rec adj nodes edges acc=
  match nodes with
  |[]->acc
  |h::t->let neighbors = edges |> List.filter (fun (u,v)-> u = h)|>List.map (fun (u,v)->v) in
          adj t edges ((h,neighbors)::acc)
  in adj nodes edges []

let reachable_count edges n u=
  let adj  = adjacency edges n in
  let rec dfs visited current acc =
    let neighbors = List.assoc current adj in
    let (visited',acc')=
    neighbors |> List.fold_left (fun (visited,acc) x ->if List.mem x visited 
     then (visited,acc)
     else (x::visited, dfs (x::visited) x (acc + 1))) (visited,acc) 
    in 
    acc'
  in dfs [u] u 1;;


